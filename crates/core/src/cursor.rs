#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{ot::EditSeq, rope::RopeExt as _};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::Rope;
use std::num::NonZeroUsize;
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
    text: W::Wrap<'a, Rope>,
    state: W::Wrap<'a, CursorState>,
}

pub type Cursor<'a> = CursorView<'a, WRef>;

pub type CursorMut<'a> = CursorView<'a, WMut>;

impl<'a, W: WrapRef> CursorView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Rope>,
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
    pub fn move_left(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.text.prev_grapheme_boundary(self.state.char_offset) {
                Some(prev) if self.state.char_offset != prev => self.state.char_offset = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.text.next_grapheme_boundary(self.state.char_offset) {
                Some(next) if self.state.char_offset != next => self.state.char_offset = next,
                _ => break,
            }
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
        edits.apply(&mut self.text).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.state.char_offset = self.text.ceil_grapheme_boundary(self.state.char_offset);
        edits
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let _ = self.delete_before_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        for _ in 1..=count.get() {
            match self.text.prev_grapheme_boundary(char_offset) {
                Some(prev) if char_offset != prev => char_offset = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(char_offset);
        edits.delete(self.state.char_offset - char_offset);
        edits.retain_rest(&self.text);
        edits.apply(&mut self.text).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        edits
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let _ = self.delete_after_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        for _ in 1..=count.get() {
            match self.text.next_grapheme_boundary(char_offset) {
                Some(next) if char_offset != next => char_offset = next,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.delete(char_offset - self.state.char_offset);
        edits.retain_rest(&self.text);
        edits.apply(&mut self.text).unwrap();
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        edits
    }
}

impl<R> TryFrom<(R, usize)> for CursorView<'_, WBox>
where
    R: Into<Rope>,
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
    use crate::rope::SnapBias;
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
            let text = Rope::new();
            let char_offset = text.snap_to_grapheme_boundary(
                u.arbitrary::<usize>()?,
                *u.choose(&[SnapBias::Before, SnapBias::After])?,
            );
            let mut cursor = CursorView::try_from((text, char_offset)).unwrap();
            let mut actions = Vec::new();
            for _ in 0..u.choose_index(100)? {
                match u.choose_index(4)? {
                    0 => {
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
                        cursor.move_left(arg);
                        actions.push(format!("move_left({arg})"));
                    }
                    1 => {
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
                        cursor.move_right(arg);
                        actions.push(format!("move_right({arg})"));
                    }
                    2 => {
                        // let arg = u.arbitrary()?; // TODO: Pass test with arbitrary Unicode
                        let arg = u.choose(&["", "x", "\t", "\n", "🇯🇵", "👨‍👨‍👧"])?;
                        cursor.insert(arg);
                        actions.push(format!("insert({arg:?})"));
                    }
                    3 => {
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
                        cursor.delete_before(arg);
                        actions.push(format!("delete_before({arg})"));
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
