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
    pub goal_column: usize,
}

impl RangeState {
    pub fn snap(&mut self, text: &Rope) {
        if self.anchor.char_offset < self.head.char_offset {
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
        Cursor::new(&self.text, &self.state.anchor)
            .expect("Range text and anchor cursor state are always kept valid")
    }

    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(&self.text, &self.state.head)
            .expect("Range text and head cursor state are always kept valid")
    }

    pub fn start(&self) -> Cursor<'_> {
        if self.state.anchor.char_offset <= self.state.head.char_offset {
            self.anchor()
        } else {
            self.head()
        }
    }

    pub fn end(&self) -> Cursor<'_> {
        if self.state.anchor.char_offset <= self.state.head.char_offset {
            self.head()
        } else {
            self.anchor()
        }
    }

    pub fn goal_column(&self) -> usize {
        self.state.goal_column
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
        self.state.anchor.char_offset == self.state.head.char_offset
    }

    pub fn is_eof(&self) -> bool {
        self.is_empty() && self.head().is_eof()
    }

    pub fn is_forward(&self) -> bool {
        self.state.anchor.char_offset <= self.state.head.char_offset
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
            .expect("Range text and anchor cursor state are always kept valid")
            .guard()
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head)
            .expect("Range text and head cursor state are always kept valid")
            .guard()
    }

    fn start_mut(&mut self) -> CursorMut<'_> {
        if self.state.anchor.char_offset <= self.state.head.char_offset {
            self.anchor_mut()
        } else {
            self.head_mut()
        }
    }

    fn end_mut(&mut self) -> CursorMut<'_> {
        if self.state.anchor.char_offset <= self.state.head.char_offset {
            self.head_mut()
        } else {
            self.anchor_mut()
        }
    }

    pub fn snap(&mut self) {
        self.state.snap(&self.text);
    }

    // Should be called after performing any horizontal movement.
    pub fn update_goal_column(&mut self) {
        self.state.goal_column = self.state.head.column(&self.text);
    }

    pub fn extend_to(&mut self, char_offset: usize) {
        self.head_mut().move_to(char_offset);
        if self.is_forward() {
            self.head_mut().move_right(1);
        }
        self.update_goal_column();
    }

    pub fn extend_left(&mut self, count: usize) {
        self.head_mut().move_left(count);
        self.update_goal_column();
    }

    pub fn extend_right(&mut self, count: usize) {
        self.head_mut().move_right(count);
        self.update_goal_column();
    }

    pub fn extend_up(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        for _ in 0..count {
            if !self.head_mut().move_up(goal_column) {
                break;
            }
        }
    }

    pub fn extend_down(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        for _ in 0..count {
            if !self.head_mut().move_down(goal_column) {
                break;
            }
        }
    }

    pub fn extend_until_prev_byte(&mut self, byte: u8) {
        self.head_mut().move_to_prev_byte(byte);
        self.update_goal_column();
    }

    pub fn extend_until_next_byte(&mut self, byte: u8) {
        self.head_mut().move_to_next_byte(byte);
        self.update_goal_column();
    }

    pub fn move_until_prev_byte(&mut self, byte: u8) {
        self.reduce();
        self.head_mut().move_to_prev_byte(byte);
        self.update_goal_column();
    }

    pub fn move_onto_prev_byte(&mut self, byte: u8) {
        self.reduce();
        if self.head_mut().move_to_prev_byte(byte) {
            self.extend_left(1);
        }
        self.update_goal_column();
    }

    pub fn move_until_next_byte(&mut self, byte: u8) {
        self.reduce();
        self.head_mut().move_to_next_byte(byte);
        self.update_goal_column();
    }

    pub fn move_onto_next_byte(&mut self, byte: u8) {
        self.reduce();
        if self.head_mut().move_to_next_byte(byte) {
            self.extend_right(1);
        }
        self.update_goal_column();
    }

    pub fn extend_onto_prev_byte(&mut self, byte: u8) {
        if self.head_mut().move_to_prev_byte(byte) && self.is_backward() {
            self.extend_left(1);
        }
        self.update_goal_column();
    }

    pub fn extend_onto_next_byte(&mut self, byte: u8) {
        if self.head_mut().move_to_next_byte(byte) && self.is_forward() {
            self.extend_right(1);
        }
        self.update_goal_column();
    }

    pub fn move_to(&mut self, char_offset: usize) {
        self.extend_to(char_offset);
        self.reduce();
        self.update_goal_column();
    }

    pub fn move_left(&mut self, count: usize) {
        self.extend_left(count);
        self.reduce();
    }

    pub fn move_right(&mut self, count: usize) {
        self.extend_right(count);
        self.reduce();
    }

    pub fn move_up(&mut self, count: usize) {
        self.extend_up(count);
        self.reduce();
    }

    pub fn move_down(&mut self, count: usize) {
        self.extend_down(count);
        self.reduce();
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

    pub fn reduce(&mut self) {
        if self.is_forward() {
            self.state.anchor.char_offset = self.state.head.char_offset;
            self.anchor_mut().move_left(1);
        } else {
            self.state.anchor.char_offset = self.state.head.char_offset;
            self.head_mut().move_right(1);
        }
    }

    pub fn insert_char(&mut self, char: char) -> EditSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> EditSeq {
        // Assumes reduction before entering insert mode.
        debug_assert!(self.grapheme_length() <= 1);
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let edits = self.start_mut().insert(text);
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.snap();
        self.update_goal_column();
        edits
    }

    pub fn delete_before(&mut self) -> Option<EditSeq> {
        if self.start().char_offset() == 0 {
            return None;
        }
        // Assumes reduction before entering insert mode.
        debug_assert!(self.grapheme_length() <= 1);
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let edits = self.start_mut().delete_before()?;
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.snap();
        self.update_goal_column();
        Some(edits)
    }

    pub fn delete(&mut self) -> Option<EditSeq> {
        if self.is_empty() {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start().char_offset());
        edits.delete(self.char_length());
        edits.retain_rest(&self.text);
        self.text.edit(&edits).expect("Edits are well formed");
        self.state.anchor.char_offset = edits.transform_char_offset(self.state.anchor.char_offset);
        self.state.head.char_offset = edits.transform_char_offset(self.state.head.char_offset);
        debug_assert_eq!(self.state.anchor.char_offset, self.state.head.char_offset);
        self.snap();
        self.update_goal_column();
        Some(edits)
    }

    pub fn delete_after(&mut self) -> Option<EditSeq> {
        if self.end().is_eof() {
            return None;
        }
        // Assumes reduction before entering insert mode.
        debug_assert!(self.grapheme_length() <= 1);
        let anchor = self.state.anchor.char_offset;
        let head = self.state.head.char_offset;
        let edits = self.end_mut().delete_after()?;
        self.state.anchor.char_offset = edits.transform_char_offset(anchor);
        self.state.head.char_offset = edits.transform_char_offset(head);
        self.snap();
        self.update_goal_column();
        Some(edits)
    }
}

impl<R> TryFrom<(R, usize, usize)> for RangeView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = Error;
    fn try_from((text, anchor, head): (R, usize, usize)) -> Result<Self, Self::Error> {
        let text = Box::new(text.into());
        let head = CursorState { char_offset: head };
        let state = Box::new(RangeState {
            anchor: CursorState {
                char_offset: anchor,
            },
            goal_column: head.column(&text),
            head,
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
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("e");
        range.assert_invariants().unwrap();
    }
}
