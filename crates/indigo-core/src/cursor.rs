use crate::{display_width::DisplayWidth as _, ot::EditSeq, rope::RopeExt as _, text::Text};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use std::thread;
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
    pub desired_column: usize,
}

#[must_use]
pub struct CursorView<'a, W: Wrap + WrapRef> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, CursorState>,
    /// Whether to assert invariants hold on drop.
    guard: bool,
}

pub type Cursor<'a> = CursorView<'a, WRef>;

pub type CursorMut<'a> = CursorView<'a, WMut>;

impl<'a, W: WrapRef> CursorView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, CursorState>,
    ) -> Result<Self, Error> {
        let cursor_view = CursorView {
            text,
            state,
            guard: false,
        };
        cursor_view.assert_invariants()?;
        Ok(cursor_view)
    }

    pub fn guard(mut self) -> Self {
        self.guard = true;
        self
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
    fn update_desired_column(&mut self) {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        let current_line_char_index = self.text.line_to_char(current_line_index);
        self.state.desired_column = self
            .text
            .slice(current_line_char_index..self.state.char_offset)
            .display_width();
    }

    pub fn move_left(&mut self) -> bool {
        if let Some(prev) = self.text.prev_grapheme_boundary(self.state.char_offset)
            && self.state.char_offset != prev
        {
            self.state.char_offset = prev;
            self.update_desired_column();
            true
        } else {
            false
        }
    }

    pub fn move_right(&mut self) -> bool {
        if let Some(next) = self.text.next_grapheme_boundary(self.state.char_offset)
            && self.state.char_offset != next
        {
            self.state.char_offset = next;
            self.update_desired_column();
            true
        } else {
            false
        }
    }

    pub fn move_up(&mut self) -> bool {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        if current_line_index == 0 {
            return false;
        }
        let target_line_index = current_line_index - 1;
        let target_line_char_index = self.text.line_to_char(target_line_index);
        let target_line_slice = self.text.line(target_line_index);
        let mut target_line_prefix = 0;
        let mut char_offset = target_line_char_index;
        for grapheme in target_line_slice.graphemes() {
            let grapheme_width = grapheme.display_width();
            if target_line_prefix + grapheme_width > self.state.desired_column {
                break;
            }
            target_line_prefix += grapheme_width;
            char_offset += grapheme.len_chars();
        }
        self.state.char_offset = char_offset;
        true
    }

    pub fn move_down(&mut self) -> bool {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        if self.is_eof() {
            return false;
        }
        let target_line_index = current_line_index + 1;
        if target_line_index >= self.text.len_lines_indigo() {
            self.state.char_offset = self.text.len_chars();
            return true;
        }
        let target_line_char_index = self.text.line_to_char(target_line_index);
        let target_line_slice = self.text.line(target_line_index);
        let mut target_line_prefix = 0;
        let mut char_offset = target_line_char_index;
        for grapheme in target_line_slice.graphemes() {
            let grapheme_width = grapheme.display_width();
            if target_line_prefix + grapheme_width > self.state.desired_column {
                break;
            }
            target_line_prefix += grapheme_width;
            char_offset += grapheme.len_chars();
        }
        self.state.char_offset = char_offset;
        true
    }

    pub fn move_to_prev_byte(&mut self, byte: u8) -> bool {
        if let Some(char_offset) = self.text.find_last_byte(..self.state.char_offset, byte) {
            self.state.char_offset = char_offset;
            self.move_right();
            self.update_desired_column();
            true
        } else {
            false
        }
    }

    pub fn move_to_next_byte(&mut self, byte: u8) -> bool {
        if let Some(char_offset) = self.text.find_first_byte(self.state.char_offset.., byte) {
            self.state.char_offset = char_offset;
            self.update_desired_column();
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
        self.state.char_offset = self.text.ceil_grapheme_boundary(self.state.char_offset);
        edits
    }

    pub fn delete_before(&mut self) {
        let _ = self.delete_before_impl();
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self) -> Option<EditSeq> {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(prev) = self.text.prev_grapheme_boundary(char_offset)
            && char_offset != prev
        {
            char_offset = prev;
        } else {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(char_offset);
        edits.delete(self.state.char_offset - char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.char_offset = self
            .text
            .ceil_grapheme_boundary(edits.transform_char_offset(self.state.char_offset));
        Some(edits)
    }

    pub fn delete_after(&mut self) {
        let _ = self.delete_after_impl();
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self) -> Option<EditSeq> {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(next) = self.text.next_grapheme_boundary(char_offset)
            && char_offset != next
        {
            char_offset = next;
        } else {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.delete(char_offset - self.state.char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        Some(edits)
    }
}

impl<R> TryFrom<(R, usize)> for CursorView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = Error;
    fn try_from((text, char_offset): (R, usize)) -> Result<Self, Self::Error> {
        let text = Box::new(text.into());
        let state = Box::new(CursorState {
            char_offset,
            desired_column: 0,
        });
        Self::new(text, state)
    }
}

impl<W: WrapRef> Drop for CursorView<'_, W> {
    fn drop(&mut self) {
        if self.guard && !thread::panicking() {
            self.assert_invariants().unwrap();
        }
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
                        let text = u.arbitrary()?;
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
