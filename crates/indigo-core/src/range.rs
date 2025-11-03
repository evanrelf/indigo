use crate::{
    cursor::{self, Cursor, CursorMut, CursorState},
    ot::EditSeq,
    rope::RopeExt as _,
    text::Text,
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::thread;
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
pub struct RangeView<'a, W: Wrap + WrapRef> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, RangeState>,
    /// Whether to assert invariants hold on drop.
    guard: bool,
}

pub type Range<'a> = RangeView<'a, WRef>;

pub type RangeMut<'a> = RangeView<'a, WMut>;

impl<'a, W: WrapRef> RangeView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, RangeState>,
    ) -> Result<Self, Error> {
        let range_view = RangeView {
            text,
            state,
            guard: false,
        };
        range_view.assert_invariants()?;
        Ok(range_view)
    }

    pub fn guard(mut self) -> Self {
        self.guard = true;
        self
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
        // if self.is_empty() && !self.is_eof() {
        //     return Err(Error::EmptyAndNotEof {
        //         anchor: self.state.anchor.char_offset,
        //         head: self.state.head.char_offset,
        //     });
        // }
        // if self.is_backward() && self.grapheme_length() <= 1 {
        //     return Err(Error::ReducedAndBackward {
        //         anchor: self.state.anchor.char_offset,
        //         head: self.state.head.char_offset,
        //     });
        // }
        Ok(())
    }
}

impl<W: WrapMut> RangeView<'_, W> {
    fn anchor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.anchor)
            .unwrap()
            .guard()
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head)
            .unwrap()
            .guard()
    }

    pub fn extend_left(&mut self) {
        self.head_mut().move_left();
    }

    pub fn extend_right(&mut self) {
        self.head_mut().move_right();
    }

    pub fn extend_until_prev_byte(&mut self, byte: u8) -> bool {
        let head = &mut self.state.head.char_offset;
        if let Some(char_offset) = self.text.find_last_byte(..*head, byte) {
            *head = char_offset;
            self.head_mut().move_right();
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

    pub fn reduce(&mut self) {
        self.state.anchor.char_offset = self.state.head.char_offset;
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, text: &str) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let edits = self.anchor_mut().insert_impl(text);
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.state.snap(&self.text);
    }

    pub fn delete_before(&mut self) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let Some(edits) = self.anchor_mut().delete_before_impl() else {
            return;
        };
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.state.snap(&self.text);
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
        self.state.snap(&self.text);
        debug_assert_eq!(self.state.anchor, self.state.head);
    }

    pub fn delete_after(&mut self) {
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let Some(edits) = self.head_mut().delete_after_impl() else {
            return;
        };
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.state.snap(&self.text);
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

impl<W: WrapRef> Drop for RangeView<'_, W> {
    fn drop(&mut self) {
        if self.guard && !thread::panicking() {
            self.assert_invariants().unwrap();
        }
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
        };
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("e");
        range.assert_invariants().unwrap();
    }
}
