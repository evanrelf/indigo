// TODO
#![allow(unused_variables)]

use crate::cursor::{Cursor, CursorMut, CursorState};
use ropey::Rope;

#[derive(Clone, Copy, Default)]
pub struct RangeState {
    anchor: CursorState,
    head: CursorState,
}

#[derive(Clone, Copy)]
pub struct Range<'a> {
    pub(crate) rope: &'a Rope,
    pub(crate) state: RangeState,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope) -> Self {
        Self {
            rope,
            state: RangeState::default(),
        }
    }

    #[must_use]
    pub fn from_cursor(cursor: Cursor<'a>) -> Self {
        let Cursor { rope, state } = cursor;
        Self {
            rope,
            state: RangeState {
                anchor: state,
                head: state,
            },
        }
    }
}

pub struct RangeMut<'a> {
    pub(crate) rope: &'a mut Rope,
    pub(crate) state: RangeState,
}

impl<'a> RangeMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope) -> Self {
        Self {
            rope,
            state: RangeState::default(),
        }
    }

    #[must_use]
    pub fn from_cursor(cursor: CursorMut<'a>) -> Self {
        let CursorMut { rope, state } = cursor;
        Self {
            rope,
            state: RangeState {
                anchor: state,
                head: state,
            },
        }
    }

    #[must_use]
    pub fn downgrade(self) -> Range<'a> {
        Range {
            rope: self.rope,
            state: self.state,
        }
    }

    #[must_use]
    pub fn downgraded(&self) -> Range {
        Range {
            rope: self.rope,
            state: self.state,
        }
    }

    #[must_use]
    pub fn anchor_mut(&mut self) -> CursorMut {
        CursorMut {
            rope: self.rope,
            state: self.state.anchor,
        }
    }

    #[must_use]
    pub fn head_mut(&mut self) -> CursorMut {
        CursorMut {
            rope: self.rope,
            state: self.state.head,
        }
    }

    pub fn with_anchor<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        todo!()
    }

    pub fn with_head<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        todo!()
    }

    pub fn insert_char(&mut self, char: char) {
        todo!();
    }

    pub fn insert(&mut self, string: &str) {
        todo!();
    }

    pub fn backspace(&mut self, count: usize) {
        todo!();
    }

    pub fn delete(&mut self, count: usize) {
        todo!();
    }
}

pub trait RangeExt {
    fn range_parts(&self) -> (&Rope, &RangeState);

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState);

    #[must_use]
    fn anchor(&self) -> Cursor {
        let (rope, state) = self.range_parts();
        Cursor {
            rope,
            state: state.anchor,
        }
    }

    #[must_use]
    fn head(&self) -> Cursor {
        let (rope, state) = self.range_parts();
        Cursor {
            rope,
            state: state.head,
        }
    }

    fn move_left(&mut self, distance: usize) -> bool {
        todo!()
    }

    fn move_right(&mut self, distance: usize) -> bool {
        todo!()
    }
}

impl<'a> RangeExt for Range<'a> {
    fn range_parts(&self) -> (&'a Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&'a Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}

impl<'a> RangeExt for RangeMut<'a> {
    fn range_parts(&self) -> (&Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}
