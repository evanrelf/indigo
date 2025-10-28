use crate::{
    cursor::{self, Cursor, CursorMut, CursorState},
    ot::EditSeq,
    rope::RopeExt as _,
    text::Text,
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::num::NonZeroUsize;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Empty but not at EOF (anchor={anchor}, head={head})")]
    EmptyAndNotEof { anchor: usize, head: usize },

    #[error("Reduced but not facing forward (anchor={anchor}, head={head})")]
    ReducedAndBackward { anchor: usize, head: usize },

    #[error("Error from anchor")]
    Anchor(#[source] cursor::Error),

    #[error("Error from head")]
    Head(#[source] cursor::Error),
}

#[derive(Clone, Default)]
pub struct RangeState {
    pub anchor: CursorState,
    pub head: CursorState,
}

impl RangeState {
    pub fn snap(&mut self, text: &Rope) {
        if self.anchor == self.head && text.len_chars() > 0 {
            self.head.char_offset += 1;
        }
        if self.anchor < self.head {
            self.anchor.char_offset = text.floor_grapheme_boundary(self.anchor.char_offset);
            self.head.char_offset = text.ceil_grapheme_boundary(self.head.char_offset);
        } else {
            self.head.char_offset = text.floor_grapheme_boundary(self.head.char_offset);
            self.anchor.char_offset = text.ceil_grapheme_boundary(self.anchor.char_offset);
        }
    }

    #[must_use]
    pub fn snapped(mut self, text: &Rope) -> Self {
        self.snap(text);
        self
    }

    fn both(&mut self) -> (&mut CursorState, &mut CursorState) {
        (&mut self.anchor, &mut self.head)
    }
}

#[must_use]
pub struct RangeView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, RangeState>,
}

pub type Range<'a> = RangeView<'a, WRef>;

pub type RangeMut<'a> = RangeView<'a, WMut>;

impl<'a, W: WrapRef> RangeView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, RangeState>,
    ) -> Result<Self, Error> {
        let range_view = RangeView { text, state };
        range_view.assert_invariants()?;
        Ok(range_view)
    }

    pub fn slice(&self) -> RopeSlice<'_> {
        let start = self.start().char_offset();
        let end = self.end().char_offset();
        self.text.slice(start..end)
    }

    pub fn anchor(&self) -> Cursor<'_> {
        Cursor::new(&self.text, &self.state.anchor).unwrap()
    }

    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(&self.text, &self.state.head).unwrap()
    }

    pub fn start(&self) -> Cursor<'_> {
        if self.state.anchor <= self.state.head {
            self.anchor()
        } else {
            self.head()
        }
    }

    pub fn end(&self) -> Cursor<'_> {
        if self.state.anchor <= self.state.head {
            self.head()
        } else {
            self.anchor()
        }
    }

    pub fn char_length(&self) -> usize {
        let start = self.start().char_offset();
        let end = self.end().char_offset();
        end - start
    }

    pub fn grapheme_length(&self) -> usize {
        match self.char_length() {
            0 => 0,
            1 => 1,
            _ => self.slice().graphemes().count(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.char_length() == 0
    }

    pub fn is_eof(&self) -> bool {
        self.is_empty() && self.head().is_eof()
    }

    pub fn is_forward(&self) -> bool {
        self.state.anchor <= self.state.head
    }

    pub fn is_backward(&self) -> bool {
        !self.is_forward()
    }

    pub(crate) fn assert_invariants(&self) -> Result<(), Error> {
        self.anchor().assert_invariants().map_err(Error::Anchor)?;
        self.head().assert_invariants().map_err(Error::Head)?;
        if self.is_empty() && !self.is_eof() {
            return Err(Error::EmptyAndNotEof {
                anchor: self.state.anchor.char_offset,
                head: self.state.head.char_offset,
            });
        }
        if self.is_backward() && self.grapheme_length() <= 1 {
            return Err(Error::ReducedAndBackward {
                anchor: self.state.anchor.char_offset,
                head: self.state.head.char_offset,
            });
        }
        Ok(())
    }
}

impl<W: WrapMut> RangeView<'_, W> {
    fn anchor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.anchor).unwrap()
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head).unwrap()
    }

    #[tracing::instrument(skip_all)]
    pub fn extend_left(&mut self, count: NonZeroUsize) {
        self.head_mut().move_left(count);
        if self.anchor().char_offset() == 0 && self.head().char_offset() == 0 {
            self.head_mut().move_right(NonZeroUsize::MIN);
            return;
        }
        if self.is_empty() {
            self.anchor_mut().move_right(NonZeroUsize::MIN);
            self.head_mut().move_left(NonZeroUsize::MIN);
            let grapheme_length = self.grapheme_length();
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.slice(),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length() == 1 {
            self.flip_forward();
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn extend_right(&mut self, count: NonZeroUsize) {
        self.head_mut().move_right(count);
        if self.is_empty() && !self.is_eof() {
            self.anchor_mut().move_left(NonZeroUsize::MIN);
            self.head_mut().move_right(NonZeroUsize::MIN);
            let grapheme_length = self.grapheme_length();
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.slice(),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length() == 1 {
            self.flip_forward();
        }
    }

    pub fn extend_until_prev_byte(&mut self, byte: u8) -> bool {
        let head = &mut self.state.head.char_offset;
        if let Some(char_offset) = self.text.find_last_byte(..*head, byte) {
            *head = char_offset;
            self.head_mut().move_right(NonZeroUsize::MIN);
            true
        } else {
            false
        }
    }

    pub fn extend_until_next_byte(&mut self, byte: u8) -> bool {
        let head = &mut self.state.head.char_offset;
        if let Some(char_offset) = self.text.find_first_byte(*head.., byte) {
            *head = char_offset;
            true
        } else {
            false
        }
    }

    pub fn flip(&mut self) {
        if self.is_forward() && self.grapheme_length() == 1 {
            return;
        }
        let (anchor, cursor) = self.state.both();
        std::mem::swap(anchor, cursor);
    }

    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn reduce(&mut self) {
        if self.is_eof() {
            return;
        }
        if self.is_forward() {
            self.state.anchor.char_offset = self.state.head.char_offset;
            self.anchor_mut().move_left(NonZeroUsize::MIN);
        } else {
            self.state.anchor.char_offset = self.state.head.char_offset;
            self.head_mut().move_right(NonZeroUsize::MIN);
        }
        let grapheme_length = self.grapheme_length();
        if grapheme_length != 1 {
            tracing::error!(
                grapheme_length,
                text = ?self.slice(),
                "Unexpected grapheme length != 1 after reduce",
            );
        }
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, text: &str) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        self.reduce();
        let edits = self.anchor_mut().insert_impl(text);
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.state.snap(&self.text);
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        self.reduce();
        let edits = self.anchor_mut().delete_before_impl(count);
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
    }

    pub fn delete(&mut self) {
        if self.is_empty() {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start().char_offset());
        edits.delete(self.char_length());
        edits.retain_rest(&self.text);
        self.text.edit(&edits).unwrap();
        self.state.anchor.char_offset = edits.transform_char_offset(self.state.anchor.char_offset);
        self.state.head.char_offset = edits.transform_char_offset(self.state.head.char_offset);
        debug_assert_eq!(self.state.anchor, self.state.head);
        self.extend_right(NonZeroUsize::MIN);
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        self.reduce();
        let edits = self.head_mut().delete_after_impl(count);
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
    }
}

impl<R> TryFrom<(R, usize, usize)> for RangeView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = Error;
    fn try_from((text, anchor, head): (R, usize, usize)) -> Result<Self, Self::Error> {
        let text = Box::new(text.into());
        let state = Box::new(RangeState {
            anchor: CursorState {
                char_offset: anchor,
            },
            head: CursorState { char_offset: head },
        });
        Self::new(text, state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut text = Text::from("\u{0301}");
        let mut state = RangeState {
            anchor: CursorState { char_offset: 0 },
            head: CursorState { char_offset: 0 },
        }
        .snapped(&text);
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("e");
        range.assert_invariants().unwrap();
    }
}
