use crate::{ot::EditSeq, rope::RopeExt as _, text::Text};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Char offset {char_offset} exceeds EOF at {eof_offset}")]
    ExceedsEof {
        char_offset: usize,
        eof_offset: usize,
    },

    #[error("Char offset {char_offset} does not lie on grapheme boundary")]
    NotOnGraphemeBoundary { char_offset: usize },
}

#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct CursorState {
    pub char_offset: usize,
}

#[must_use]
pub struct CursorView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, CursorState>,
}

pub type Cursor<'a> = CursorView<'a, WRef>;

pub type CursorMut<'a> = CursorView<'a, WMut>;

impl<'a, W: WrapRef> CursorView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, CursorState>,
    ) -> Result<Self, Error> {
        let cursor_view = CursorView { text, state };
        cursor_view.assert_invariants()?;
        Ok(cursor_view)
    }

    #[must_use]
    pub fn char_offset(&self) -> usize {
        self.state.char_offset
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.state.char_offset == self.text.len_chars()
    }

    pub(crate) fn assert_invariants(&self) -> Result<(), Error> {
        if self.state.char_offset > self.text.len_chars() {
            return Err(Error::ExceedsEof {
                char_offset: self.state.char_offset,
                eof_offset: self.text.len_chars(),
            });
        }
        if !self.text.is_grapheme_boundary(self.state.char_offset) {
            return Err(Error::NotOnGraphemeBoundary {
                char_offset: self.state.char_offset,
            });
        }
        Ok(())
    }
}

impl<W: WrapMut> CursorView<'_, W> {
    pub fn move_left(&mut self) {
        if let Some(prev) = self.text.prev_grapheme_boundary(self.state.char_offset)
            && self.state.char_offset != prev
        {
            self.state.char_offset = prev;
        }
    }

    pub fn move_right(&mut self) {
        if let Some(next) = self.text.next_grapheme_boundary(self.state.char_offset)
            && self.state.char_offset != next
        {
            self.state.char_offset = next;
        }
    }

    pub fn move_to_prev_byte(&mut self, byte: u8) -> bool {
        if let Some(char_offset) = self.text.find_last_byte(..self.state.char_offset, byte) {
            self.state.char_offset = char_offset;
            self.move_right();
            true
        } else {
            false
        }
    }

    pub fn move_to_next_byte(&mut self, byte: u8) -> bool {
        if let Some(char_offset) = self.text.find_first_byte(self.state.char_offset.., byte) {
            self.state.char_offset = char_offset;
            true
        } else {
            false
        }
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, text: &str) {
        let _ = self.insert_impl(text);
    }

    #[must_use]
    pub(crate) fn insert_impl(&mut self, text: &str) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.insert(text);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.state.char_offset = self.text.ceil_grapheme_boundary(self.state.char_offset);
        edits
    }

    pub fn delete_before(&mut self) {
        let _ = self.delete_before_impl();
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(prev) = self.text.prev_grapheme_boundary(char_offset)
            && char_offset != prev
        {
            char_offset = prev;
        }
        let mut edits = EditSeq::new();
        edits.retain(char_offset);
        edits.delete(self.state.char_offset - char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        edits
    }

    pub fn delete_after(&mut self) {
        let _ = self.delete_after_impl();
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(next) = self.text.next_grapheme_boundary(char_offset)
            && char_offset != next
        {
            char_offset = next;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.delete(char_offset - self.state.char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        edits
    }
}

impl<R> TryFrom<(R, usize)> for CursorView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = Error;
    fn try_from((text, char_offset): (R, usize)) -> Result<Self, Self::Error> {
        let text = Box::new(text.into());
        let state = Box::new(CursorState { char_offset });
        Self::new(text, state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;
    use std::cmp::max;

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        cursor.insert("e");
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let text = Text::new();
            let char_offset = if u.arbitrary::<bool>()? {
                text.floor_grapheme_boundary(u.arbitrary::<usize>()?)
            } else {
                text.ceil_grapheme_boundary(u.arbitrary::<usize>()?)
            };
            let mut cursor = CursorView::try_from((text, char_offset)).unwrap();
            let mut actions = Vec::new();
            for _ in 0..u.choose_index(100)? {
                match u.choose_index(4)? {
                    0 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.move_left();
                        }
                        actions.push(format!("move_left() x{count}"));
                    }
                    1 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.move_right();
                        }
                        actions.push(format!("move_right() x{count}"));
                    }
                    2 => {
                        // let text = u.arbitrary()?; // TODO: Pass test with arbitrary Unicode
                        let text = u.choose(&["", "x", "\t", "\n", "🇯🇵", "👨‍👨‍👧"])?;
                        cursor.insert(text);
                        actions.push(format!("insert({text:?})"));
                    }
                    3 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.delete_before();
                        }
                        actions.push(format!("delete_before() x{count}"));
                    }
                    _ => break,
                }
                let actions = actions.join("\n  ");
                let _ = actions;
                cursor.assert_invariants().unwrap();
            }
            Ok(())
        });
    }
}
