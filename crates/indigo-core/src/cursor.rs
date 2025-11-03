use crate::{ot::EditSeq, rope::RopeExt as _, text::Text};
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
    pub fn move_left(&mut self) -> bool {
        if let Some(prev) = self.text.prev_grapheme_boundary(self.state.char_offset)
            && self.state.char_offset != prev
        {
            self.state.char_offset = prev;
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
            true
        } else {
            false
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

    pub fn insert(&mut self, text: &str) -> EditSeq {
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

    // Behavior traditionally associated with the Backspace key.
    pub fn delete_before(&mut self) -> Option<EditSeq> {
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

    // Behavior traditionally associated with the Delete key.
    pub fn delete_after(&mut self) -> Option<EditSeq> {
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
        self.state.char_offset = self
            .text
            .ceil_grapheme_boundary(edits.transform_char_offset(self.state.char_offset));
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
        let state = Box::new(CursorState { char_offset });
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
    use std::{cmp::max, sync::mpsc, thread};

    macro_rules! defer {
        ($f:expr) => {
            let __defer = {
                struct Defer<F: FnMut()>(F);
                impl<F: FnMut()> Drop for Defer<F> {
                    fn drop(&mut self) {
                        (self.0)();
                    }
                }
                Defer($f)
            };
        };
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        cursor.insert("e");
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn expected_behavior() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        cursor.insert("hello");
        assert_eq!(*cursor.text, Text::from("hello"));
        assert_eq!(cursor.char_offset(), 5);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(*cursor.text, Text::from("hel"));
        assert_eq!(cursor.char_offset(), 3);

        cursor.move_left();
        cursor.move_left();
        assert_eq!(cursor.char_offset(), 1);

        cursor.delete_after();
        assert_eq!(cursor.char_offset(), 1);
        assert_eq!(*cursor.text, Text::from("hl"));

        cursor.move_right();
        assert_eq!(cursor.char_offset(), 2);
        cursor.move_right();
        assert_eq!(cursor.char_offset(), 2);

        cursor.delete_after();
        assert_eq!(*cursor.text, Text::from("hl"));
        assert_eq!(cursor.char_offset(), 2);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(*cursor.text, Text::from(""));
        assert_eq!(cursor.char_offset(), 0);

        cursor.delete_before();
        assert_eq!(*cursor.text, Text::from(""));
        assert_eq!(cursor.char_offset(), 0);
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
            let (tx, rx) = mpsc::channel();
            defer!(|| {
                if thread::panicking() {
                    let actions: Vec<String> = rx.try_iter().collect();
                    if !actions.is_empty() {
                        eprintln!("Actions:\n  {}", actions.join("\n  "));
                    }
                }
            });
            for _ in 0..u.choose_index(100)? {
                match u.choose_index(5)? {
                    0 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.move_left();
                        }
                        tx.send(format!("move_left() x{count}")).unwrap();
                    }
                    1 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.move_right();
                        }
                        tx.send(format!("move_right() x{count}")).unwrap();
                    }
                    2 => {
                        let text = u.arbitrary()?;
                        cursor.insert(text);
                        tx.send(format!("insert({text:?})")).unwrap();
                    }
                    3 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.delete_before();
                        }
                        tx.send(format!("delete_before() x{count}")).unwrap();
                    }
                    4 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.delete_after();
                        }
                        tx.send(format!("delete_after() x{count}")).unwrap();
                    }
                    _ => unreachable!(),
                }
                cursor.assert_invariants().unwrap();
            }
            Ok(())
        });
    }
}
