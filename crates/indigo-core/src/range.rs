use crate::{
    cursor::{Cursor, CursorMut, CursorState},
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
    /// The column the head _wants_ to be on. Saved to restore horizontal positioning through
    /// vertical movements. Reset on any non-vertical movement.
    pub goal_column: usize,
}

impl RangeState {
    /// Snap anchor and head outward to nearest grapheme boundaries. This is a no-op if the range is
    /// already valid.
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

    /// An empty range is considered forward.
    pub fn is_forward(&self) -> bool {
        self.state.anchor.char_offset <= self.state.head.char_offset
    }

    pub fn is_backward(&self) -> bool {
        !self.is_forward()
    }

    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        self.anchor().assert_invariants().map_err(Error::Anchor)?;
        self.head().assert_invariants().map_err(Error::Head)?;
        // TODO: Restore invariants once positioning is rock solid.
        // if self.is_empty() && !self.head().is_at_end() {
        //     return Err(Error::EmptyAndNotEnd {
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
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head)
            .expect("Range text and head cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
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

    /// Should be called after performing any non-vertical movement.
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

    pub fn move_to(&mut self, char_offset: usize) {
        self.extend_to(char_offset);
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_left(&mut self, count: usize) {
        self.head_mut().move_left(count);
        self.update_goal_column();
    }

    pub fn move_left(&mut self, count: usize) {
        self.extend_left(count);
        self.reduce();
    }

    pub fn extend_right(&mut self, count: usize) {
        self.head_mut().move_right(count);
        self.update_goal_column();
    }

    pub fn move_right(&mut self, count: usize) {
        self.extend_right(count);
        self.reduce();
    }

    pub fn extend_up(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        self.head_mut().move_up(goal_column, count);
    }

    pub fn move_up(&mut self, count: usize) {
        self.extend_up(count);
        self.reduce();
    }

    pub fn extend_down(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        self.head_mut().move_down(goal_column, count);
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
        self.head_mut().move_to_bottom();
    }

    pub fn move_to_bottom(&mut self) {
        self.extend_to_bottom();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_start(&mut self) {
        self.head_mut().move_to_line_start();
    }

    pub fn move_to_line_start(&mut self) {
        self.extend_to_line_start();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_non_blank_start(&mut self) {
        self.head_mut().move_to_line_non_blank_start();
    }

    pub fn move_to_line_non_blank_start(&mut self) {
        self.extend_to_line_non_blank_start();
        self.reduce();
        self.update_goal_column();
    }

    pub fn extend_to_line_end(&mut self) {
        self.head_mut().move_to_line_end();
    }

    pub fn move_to_line_end(&mut self) {
        self.extend_to_line_end();
        self.reduce();
        self.update_goal_column();
    }

    pub fn flip(&mut self) {
        fn both(state: &mut RangeState) -> (&mut CursorState, &mut CursorState) {
            (&mut state.anchor, &mut state.head)
        }
        if self.is_forward() && self.grapheme_length() == 1 {
            return;
        }
        let (anchor, cursor) = both(&mut self.state);
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
            self.state.anchor.char_offset = self.state.head.char_offset;
            self.head_mut().move_right(1);
        } else {
            self.state.anchor.char_offset = self.state.head.char_offset;
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
        if self.start().is_at_start() {
            return None;
        }
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
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
        if self.end().is_at_end() {
            return None;
        }
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
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
    type Error = anyhow::Error;
    fn try_from((text, anchor, head): (R, usize, usize)) -> anyhow::Result<Self> {
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
        let first_anchor_offset = range.anchor().char_offset();
        let first_head_offset = range.head().char_offset();
        assert_eq!(&range.slice().to_string(), "y");

        range.move_to_line_start();
        let second_anchor_offset = range.anchor().char_offset();
        let second_head_offset = range.head().char_offset();
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
        let first_anchor_offset = range.anchor().char_offset();
        let first_head_offset = range.head().char_offset();
        assert_eq!(&range.slice().to_string(), "x");

        range.move_to_line_non_blank_start();
        let second_anchor_offset = range.anchor().char_offset();
        let second_head_offset = range.head().char_offset();
        assert_eq!(first_anchor_offset, second_anchor_offset);
        assert_eq!(first_head_offset, second_head_offset);
        assert_eq!(&range.slice().to_string(), "x");
    }
}
