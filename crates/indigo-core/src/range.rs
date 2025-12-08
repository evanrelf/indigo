use crate::{
    cursor::{Affinity, Cursor, CursorMut, CursorState},
    edit::EditSeq,
    rope::RopeExt as _,
    text::Text,
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::{mem, thread};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Empty but not at end of text (anchor={anchor}, head={head})")]
    EmptyAndNotEnd { anchor: usize, head: usize },

    #[error("Reduced but not facing forward (anchor={anchor}, head={head})")]
    ReducedAndBackward { anchor: usize, head: usize },

    #[error("Error from anchor")]
    Anchor(#[source] anyhow::Error),

    #[error("Error from head")]
    Head(#[source] anyhow::Error),
}

#[derive(Clone, Default)]
pub struct RangeState {
    pub anchor: CursorState,
    pub head: CursorState,
    /// The display column the head _wants_ to be on. Saved to restore horizontal positioning
    /// through vertical movements. Reset on any non-vertical movement.
    pub goal_column: usize,
}

impl RangeState {
    /// Snap anchor and head outward to nearest grapheme boundaries. This is a no-op if the range is
    /// already valid.
    pub fn snap_to_grapheme_boundaries(&mut self, text: &Rope) {
        if self.anchor.byte_offset < self.head.byte_offset {
            self.anchor.byte_offset = text.floor_grapheme_boundary(self.anchor.byte_offset);
            self.head.byte_offset = text.ceil_grapheme_boundary(self.head.byte_offset);
        } else {
            self.head.byte_offset = text.floor_grapheme_boundary(self.head.byte_offset);
            self.anchor.byte_offset = text.ceil_grapheme_boundary(self.anchor.byte_offset);
        }
    }

    #[must_use]
    pub fn snapped_to_grapheme_boundaries(mut self, text: &Rope) -> Self {
        self.snap_to_grapheme_boundaries(text);
        self
    }

    pub fn transform(&mut self, edits: &EditSeq) {
        self.anchor.transform(edits);
        self.head.transform(edits);
    }

    #[must_use]
    pub fn start(&self) -> &CursorState {
        if self.anchor.byte_offset <= self.head.byte_offset {
            &self.anchor
        } else {
            &self.head
        }
    }

    #[must_use]
    pub fn end(&self) -> &CursorState {
        if self.anchor.byte_offset <= self.head.byte_offset {
            &self.head
        } else {
            &self.anchor
        }
    }

    #[must_use]
    pub fn is_touching(&self, other: &Self) -> bool {
        let self_start = self.start().byte_offset;
        let self_end = self.start().byte_offset;
        let other_start = other.start().byte_offset;
        let other_end = other.end().byte_offset;
        self_end == other_start || other_end == self_start
    }

    #[must_use]
    pub fn is_overlapping(&self, other: &Self) -> bool {
        let self_start = self.start().byte_offset;
        let self_end = self.start().byte_offset;
        let other_start = other.start().byte_offset;
        let other_end = other.end().byte_offset;
        (self_start < other_start && other_start < self_end)
            || (other_start < self_start && self_start < other_end)
    }
}

#[must_use]
pub struct RangeView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, RangeState>,
    #[expect(clippy::type_complexity)]
    on_drop: Option<Box<dyn FnOnce(&mut Self) + 'a>>,
}

pub type Range<'a> = RangeView<'a, WRef>;

pub type RangeMut<'a> = RangeView<'a, WMut>;

impl<'a, W: Wrap> RangeView<'a, W> {
    pub fn on_drop(mut self, f: impl FnOnce(&mut Self) + 'a) -> Self {
        self.on_drop = Some(Box::new(f));
        self
    }
}

impl<'a, W: WrapRef> RangeView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, RangeState>,
    ) -> anyhow::Result<Self> {
        let range_view = RangeView {
            text,
            state,
            on_drop: None,
        };
        range_view.assert_invariants()?;
        Ok(range_view)
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn state(&self) -> &RangeState {
        &self.state
    }

    pub fn slice(&self) -> RopeSlice<'_> {
        let start = self.start().byte_offset();
        let end = self.end().byte_offset();
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
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.anchor()
        } else {
            self.head()
        }
    }

    pub fn end(&self) -> Cursor<'_> {
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.head()
        } else {
            self.anchor()
        }
    }

    #[must_use]
    pub fn anchor_affinity(&self) -> Affinity {
        if self.is_forward() {
            Affinity::After
        } else {
            Affinity::Before
        }
    }

    #[must_use]
    pub fn head_affinity(&self) -> Affinity {
        if self.is_forward() {
            Affinity::Before
        } else {
            Affinity::After
        }
    }

    #[must_use]
    pub fn start_affinity(&self) -> Affinity {
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.anchor_affinity()
        } else {
            self.head_affinity()
        }
    }

    #[must_use]
    pub fn end_affinity(&self) -> Affinity {
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.head_affinity()
        } else {
            self.anchor_affinity()
        }
    }

    pub fn goal_column(&self) -> usize {
        self.state.goal_column
    }

    pub fn byte_length(&self) -> usize {
        let start = self.start().byte_offset();
        let end = self.end().byte_offset();
        end - start
    }

    pub fn grapheme_length(&self) -> usize {
        match self.byte_length() {
            0 => 0,
            1 => 1,
            _ => self.slice().graphemes().count(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.state.anchor.byte_offset == self.state.head.byte_offset
    }

    /// An empty range is considered forward.
    pub fn is_forward(&self) -> bool {
        self.state.anchor.byte_offset <= self.state.head.byte_offset
    }

    pub fn is_backward(&self) -> bool {
        !self.is_forward()
    }

    pub fn is_touching<W2>(&self, other: &RangeView<'_, W2>) -> bool
    where
        W2: WrapRef,
    {
        self.state.is_touching(&other.state)
    }

    pub fn is_overlapping<W2>(&self, other: &RangeView<'_, W2>) -> bool
    where
        W2: WrapRef,
    {
        self.state.is_overlapping(&other.state)
    }

    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        self.anchor().assert_invariants().map_err(Error::Anchor)?;
        self.head().assert_invariants().map_err(Error::Head)?;
        // TODO: Restore invariants once positioning is rock solid.
        // if self.is_empty() && !self.head().is_at_end() {
        //     return Err(Error::EmptyAndNotEnd {
        //         anchor: self.state.anchor.byte_offset,
        //         head: self.state.head.byte_offset,
        //     });
        // }
        // if self.is_backward() && self.grapheme_length() <= 1 {
        //     return Err(Error::ReducedAndBackward {
        //         anchor: self.state.anchor.byte_offset,
        //         head: self.state.head.byte_offset,
        //     });
        // }
        Ok(())
    }
}

impl<W: WrapMut> RangeView<'_, W> {
    fn anchor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.anchor)
            .expect("Range text and anchor cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head)
            .expect("Range text and head cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    fn start_mut(&mut self) -> CursorMut<'_> {
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.anchor_mut()
        } else {
            self.head_mut()
        }
    }

    fn end_mut(&mut self) -> CursorMut<'_> {
        if self.state.anchor.byte_offset <= self.state.head.byte_offset {
            self.head_mut()
        } else {
            self.anchor_mut()
        }
    }

    pub fn snap_to_grapheme_boundaries(&mut self) {
        self.state.snap_to_grapheme_boundaries(&self.text);
    }

    /// Should be called after performing any non-vertical movement.
    pub fn update_goal_column(&mut self) {
        let head_column = self
            .head()
            .display_column(self.head_affinity())
            .unwrap_or(0);
        self.state.goal_column = head_column;
    }

    pub fn extend_to(&mut self, byte_offset: usize) {
        self.head_mut().move_to(byte_offset);
        if self.is_forward() {
            self.head_mut().move_right(1);
        }
        self.update_goal_column();
    }

    pub fn move_to(&mut self, byte_offset: usize) {
        self.extend_to(byte_offset);
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_left(&mut self, count: usize) {
        if self.grapheme_length() == 1 {
            self.unchecked_flip_backward();
        }
        self.head_mut().move_left(count);
        self.update_goal_column();
    }

    pub fn move_left(&mut self, count: usize) {
        self.extend_left(count);
        self.reduce();
    }

    pub fn extend_right(&mut self, count: usize) {
        if self.grapheme_length() == 1 {
            self.flip_forward();
        }
        self.head_mut().move_right(count);
        self.update_goal_column();
    }

    pub fn move_right(&mut self, count: usize) {
        self.extend_right(count);
        self.reduce();
    }

    pub fn extend_up(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        let affinity = self.head_affinity();
        self.head_mut().move_up(goal_column, affinity, count);
    }

    pub fn move_up(&mut self, count: usize) {
        self.extend_up(count);
        self.reduce();
    }

    pub fn extend_down(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        let affinity = self.head_affinity();
        self.head_mut().move_down(goal_column, affinity, count);
    }

    pub fn move_down(&mut self, count: usize) {
        self.extend_down(count);
        self.reduce();
    }

    pub fn extend_until_prev_byte(&mut self, byte: u8, count: usize) {
        self.head_mut().move_to_prev_byte(byte, count);
        self.update_goal_column();
    }

    pub fn move_until_prev_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.head_mut().move_to_prev_byte(byte, count);
        self.update_goal_column();
    }

    pub fn extend_onto_prev_byte(&mut self, byte: u8, count: usize) {
        if self.head_mut().move_to_prev_byte(byte, count) && self.is_backward() {
            self.extend_left(1);
        }
        self.update_goal_column();
    }

    pub fn move_onto_prev_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        if self.head_mut().move_to_prev_byte(byte, count) {
            self.extend_left(1);
        }
        self.update_goal_column();
    }

    pub fn extend_until_next_byte(&mut self, byte: u8, count: usize) {
        self.head_mut().move_to_next_byte(byte, count);
        self.update_goal_column();
    }

    pub fn move_until_next_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.head_mut().move_to_next_byte(byte, count);
        self.update_goal_column();
    }

    pub fn extend_onto_next_byte(&mut self, byte: u8, count: usize) {
        if self.head_mut().move_to_next_byte(byte, count) && self.is_forward() {
            self.extend_right(1);
        }
        self.update_goal_column();
    }

    pub fn move_onto_next_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        if self.head_mut().move_to_next_byte(byte, count) {
            self.extend_right(1);
        }
        self.update_goal_column();
    }

    pub fn extend_to_start(&mut self) {
        self.head_mut().move_to_start();
    }

    pub fn move_to_start(&mut self) {
        self.extend_to_start();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_end(&mut self) {
        self.head_mut().move_to_end();
    }

    pub fn move_to_end(&mut self) {
        self.extend_to_end();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_bottom(&mut self) {
        let affinity = self.head_affinity();
        self.head_mut().move_to_bottom(affinity);
    }

    pub fn move_to_bottom(&mut self) {
        self.extend_to_bottom();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_start(&mut self) {
        let affinity = self.head_affinity();
        self.head_mut().move_to_line_start(affinity);
    }

    pub fn move_to_line_start(&mut self) {
        self.extend_to_line_start();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_non_blank_start(&mut self) {
        let affinity = self.head_affinity();
        self.head_mut().move_to_line_non_blank_start(affinity);
    }

    pub fn move_to_line_non_blank_start(&mut self) {
        self.extend_to_line_non_blank_start();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_end(&mut self) {
        let affinity = self.head_affinity();
        self.head_mut().move_to_line_end(affinity);
    }

    pub fn move_to_line_end(&mut self) {
        self.extend_to_line_end();
        self.reduce();
        self.update_goal_column();
    }

    pub fn flip(&mut self) {
        if self.is_forward() && self.grapheme_length() == 1 {
            return;
        }
        self.unchecked_flip();
    }

    fn unchecked_flip(&mut self) {
        fn both(state: &mut RangeState) -> (&mut CursorState, &mut CursorState) {
            (&mut state.anchor, &mut state.head)
        }
        let (anchor, cursor) = both(&mut self.state);
        mem::swap(&mut anchor.byte_offset, &mut cursor.byte_offset);
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

    fn unchecked_flip_backward(&mut self) {
        if self.is_forward() {
            self.unchecked_flip();
        }
    }

    pub fn reduce(&mut self) {
        if self.is_empty() {
            // Too small -> expand
            if self.head().is_at_end() {
                // Expand from end
                self.anchor_mut().move_left(1);
            } else {
                // Expand from start or middle
                self.head_mut().move_right(1);
            }
            return;
        }

        if self.grapheme_length() == 1 {
            // Just right
            self.flip_forward();
            return;
        }

        // Too big -> contract
        if self.is_backward() {
            self.state.anchor.byte_offset = self.state.head.byte_offset;
            self.head_mut().move_right(1);
        } else {
            self.state.anchor.byte_offset = self.state.head.byte_offset;
            self.anchor_mut().move_left(1);
        }
    }

    pub fn insert_char(&mut self, char: char) -> EditSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> EditSeq {
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let anchor = self.state.anchor.byte_offset;
        let head = self.state.head.byte_offset;
        let edits = self.start_mut().insert(text);
        self.state.anchor.byte_offset = edits.transform_byte_offset(anchor);
        self.state.head.byte_offset = edits.transform_byte_offset(head);
        self.snap_to_grapheme_boundaries();
        self.update_goal_column();
        edits
    }

    pub fn delete_before(&mut self) -> Option<EditSeq> {
        if self.start().is_at_start() {
            return None;
        }
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let anchor = self.state.anchor.byte_offset;
        let head = self.state.head.byte_offset;
        let edits = self.start_mut().delete_before()?;
        self.state.anchor.byte_offset = edits.transform_byte_offset(anchor);
        self.state.head.byte_offset = edits.transform_byte_offset(head);
        self.snap_to_grapheme_boundaries();
        self.update_goal_column();
        Some(edits)
    }

    pub fn delete(&mut self) -> Option<EditSeq> {
        if self.is_empty() {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start().byte_offset());
        edits.delete(self.byte_length());
        edits.retain_rest(&self.text);
        self.text.edit(&edits).expect("Edits are well formed");
        self.state.anchor.byte_offset = edits.transform_byte_offset(self.state.anchor.byte_offset);
        self.state.head.byte_offset = edits.transform_byte_offset(self.state.head.byte_offset);
        debug_assert_eq!(self.state.anchor.byte_offset, self.state.head.byte_offset);
        self.snap_to_grapheme_boundaries();
        self.update_goal_column();
        Some(edits)
    }

    pub fn delete_after(&mut self) -> Option<EditSeq> {
        if self.end().is_at_end() {
            return None;
        }
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let anchor = self.state.anchor.byte_offset;
        let head = self.state.head.byte_offset;
        let edits = self.end_mut().delete_after()?;
        self.state.anchor.byte_offset = edits.transform_byte_offset(anchor);
        self.state.head.byte_offset = edits.transform_byte_offset(head);
        self.snap_to_grapheme_boundaries();
        self.update_goal_column();
        Some(edits)
    }
}

impl<R> TryFrom<(R, usize, usize)> for RangeView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = anyhow::Error;
    fn try_from((text, anchor, head): (R, usize, usize)) -> anyhow::Result<Self> {
        let text = Box::new(text.into());
        let head = CursorState { byte_offset: head };
        let state = Box::new(RangeState {
            anchor: CursorState {
                byte_offset: anchor,
            },
            goal_column: 0,
            head,
        });
        Self::new(text, state).map(|mut range| {
            range.update_goal_column();
            range
        })
    }
}

impl<W: Wrap> Drop for RangeView<'_, W> {
    fn drop(&mut self) {
        if !thread::panicking()
            && let Some(f) = self.on_drop.take()
        {
            f(self);
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

    #[test]
    fn move_to_line_start_from_newline() {
        let mut text = Text::from("hello world\n");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.move_to_line_end();
        range.move_right(1);
        assert_eq!(&range.slice().to_string(), "\n");
        range.move_to_line_start();
        assert_eq!(&range.slice().to_string(), "h");
    }

    #[test]
    fn move_to_line_start_idempotent() {
        let mut text = Text::from("");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("x\ny");

        range.move_to_line_start();
        let first_anchor_offset = range.anchor().byte_offset();
        let first_head_offset = range.head().byte_offset();
        assert_eq!(&range.slice().to_string(), "y");

        range.move_to_line_start();
        let second_anchor_offset = range.anchor().byte_offset();
        let second_head_offset = range.head().byte_offset();
        assert_eq!(first_anchor_offset, second_anchor_offset);
        assert_eq!(first_head_offset, second_head_offset);
        assert_eq!(&range.slice().to_string(), "y");
    }

    #[test]
    fn move_to_line_non_blank_start_idempotent() {
        let mut text = Text::from("");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert(" x");

        range.move_to_line_non_blank_start();
        let first_anchor_offset = range.anchor().byte_offset();
        let first_head_offset = range.head().byte_offset();
        assert_eq!(&range.slice().to_string(), "x");

        range.move_to_line_non_blank_start();
        let second_anchor_offset = range.anchor().byte_offset();
        let second_head_offset = range.head().byte_offset();
        assert_eq!(first_anchor_offset, second_anchor_offset);
        assert_eq!(first_head_offset, second_head_offset);
        assert_eq!(&range.slice().to_string(), "x");
    }
}
