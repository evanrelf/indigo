use crate::{
    cursor::{Cursor, CursorExt, CursorMut, CursorState},
    ot::EditSeq,
};
use anyhow::Context as _;
use ropey::Rope;

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct RangeState {
    anchor: CursorState,
    head: CursorState,
}

#[derive(Clone, Copy, Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    state: RangeState,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope) -> Self {
        Self {
            rope,
            state: RangeState::default(),
        }
    }

    pub(crate) fn from_state(rope: &'a Rope, state: RangeState) -> anyhow::Result<Self> {
        let anchor = Cursor::from_state(rope, state.anchor)
            .ok()
            .context("Failed to create Range anchor from state")?
            .into_state();

        let head = Cursor::from_state(rope, state.head)
            .ok()
            .context("Failed to create Range head from state")?
            .into_state();

        let state = RangeState { anchor, head };

        Ok(Self { rope, state })
    }

    pub(crate) fn into_state(self) -> RangeState {
        self.state
    }
}

#[derive(Debug)]
pub struct RangeMut<'a> {
    rope: &'a mut Rope,
    state: RangeState,
}

impl<'a> RangeMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope) -> Self {
        Self {
            rope,
            state: RangeState::default(),
        }
    }

    pub(crate) fn from_state(rope: &'a mut Rope, state: RangeState) -> anyhow::Result<Self> {
        let anchor = Cursor::from_state(rope, state.anchor)
            .ok()
            .context("Failed to create RangeMut anchor from state")?
            .into_state();

        let head = Cursor::from_state(rope, state.head)
            .ok()
            .context("Failed to create RangeMut head from state")?
            .into_state();

        let state = RangeState { anchor, head };

        Ok(Self { rope, state })
    }

    pub(crate) fn into_state(self) -> RangeState {
        self.state
    }

    #[must_use]
    fn anchor_mut(&mut self) -> CursorMut {
        let Self { rope, state } = self;
        CursorMut::from_state(rope, state.anchor).unwrap()
    }

    #[must_use]
    fn head_mut(&mut self) -> CursorMut {
        let Self { rope, state } = self;
        CursorMut::from_state(rope, state.head).unwrap()
    }

    fn with_anchor_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let mut anchor = self.anchor_mut();
        let result = func(&mut anchor);
        self.state.anchor = anchor.into_state();
        result
    }

    fn with_head_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let mut head = self.head_mut();
        let result = func(&mut head);
        self.state.head = head.into_state();
        result
    }

    pub fn insert_char(&mut self, char: char) {
        let mut edits = EditSeq::new();
        edits.retain(self.state.head.char_index);
        edits.insert(char.to_string());
        edits.retain(self.rope.len_chars() - self.state.head.char_index);
        edits.apply(self.rope).unwrap();
        self.state.anchor.char_index = edits.transform_index(self.state.anchor.char_index);
        self.state.head.char_index = edits.transform_index(self.state.head.char_index);
    }

    pub fn insert(&mut self, string: &str) {
        let mut edits = EditSeq::new();
        edits.retain(self.state.head.char_index);
        edits.insert(string);
        edits.retain(self.rope.len_chars() - self.state.head.char_index);
        edits.apply(self.rope).unwrap();
        self.state.anchor.char_index = edits.transform_index(self.state.anchor.char_index);
        self.state.head.char_index = edits.transform_index(self.state.head.char_index);
    }

    // TODO: Move anchor into valid position
    pub fn backspace(&mut self, count: usize) {
        self.with_head_mut(|cursor| cursor.backspace(count));
    }

    // TODO: Disambiguate `Delete` key and deleting the contents of the `Range`
    pub fn delete(&mut self, count: usize) {
        self.with_head_mut(|cursor| cursor.delete(count));
    }
}

trait RangeParts {
    fn range_parts(&self) -> (&Rope, &RangeState);

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState);
}

impl RangeParts for Range<'_> {
    fn range_parts(&self) -> (&Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}

impl RangeParts for RangeMut<'_> {
    fn range_parts(&self) -> (&Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}

#[allow(private_bounds)]
pub trait RangeExt: RangeParts {
    #[must_use]
    fn anchor(&self) -> Cursor {
        let (rope, state) = self.range_parts();
        Cursor::from_state(rope, state.anchor).unwrap()
    }

    #[must_use]
    fn head(&self) -> Cursor {
        let (rope, state) = self.range_parts();
        Cursor::from_state(rope, state.head).unwrap()
    }

    fn with_anchor<T>(&mut self, func: impl Fn(&mut Cursor) -> T) -> T {
        let mut anchor = self.anchor();
        let result = func(&mut anchor);
        let anchor_state = anchor.into_state();
        let (_rope, state) = self.range_parts_mut();
        state.anchor = anchor_state;
        result
    }

    fn with_head<T>(&mut self, func: impl Fn(&mut Cursor) -> T) -> T {
        let mut head = self.head();
        let result = func(&mut head);
        let head_state = head.into_state();
        let (_rope, state) = self.range_parts_mut();
        state.head = head_state;
        result
    }

    fn move_left(&mut self, distance: usize) -> bool {
        let moved = self.extend_left(distance);
        self.reduce();
        moved
    }

    fn move_right(&mut self, distance: usize) -> bool {
        let moved = self.extend_right(distance);
        self.reduce();
        moved
    }

    fn extend_left(&mut self, distance: usize) -> bool {
        self.with_head(|cursor| cursor.move_left(distance))
    }

    fn extend_right(&mut self, distance: usize) -> bool {
        self.with_head(|cursor| cursor.move_right(distance))
    }

    fn reduce(&mut self) {
        let (_rope, state) = self.range_parts_mut();
        state.anchor = state.head;
    }
}

impl<T> RangeExt for T where T: RangeParts {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bug() {
        let mut rope = Rope::from("foo\n");
        let mut range = RangeMut::new(&mut rope);
        range.move_right(3);
        assert_eq!(range.anchor().char(), Some('\n'));
        assert_eq!(range.head().char(), Some('\n'));
        range.move_right(1);
        assert_eq!(range.anchor().char(), None);
        assert_eq!(range.head().char(), None);
        range.backspace(1);
        // TODO 1: Make this test fail
        // TODO 2: Fix the bug this test demonstrates
    }
}
