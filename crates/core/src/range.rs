// TODO
#![allow(unused_variables)]

use crate::cursor::{Cursor, CursorExt, CursorMut, CursorState};
use ropey::Rope;

#[derive(Clone, Copy, Default)]
pub struct RangeState {
    anchor: CursorState,
    head: CursorState,
}

#[derive(Clone, Copy)]
pub struct Range<'a> {
    pub(crate) rope: &'a Rope,
    pub(crate) state: &'a RangeState,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn anchor(&self) -> Cursor {
        let Self { rope, state } = self;
        Cursor {
            rope,
            state: state.anchor,
        }
    }

    #[must_use]
    pub fn head(&self) -> Cursor {
        let Self { rope, state } = self;
        Cursor {
            rope,
            state: state.head,
        }
    }
}

pub struct RangeMut<'a> {
    pub(crate) rope: &'a mut Rope,
    pub(crate) state: &'a mut RangeState,
}

impl<'a> RangeMut<'a> {
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
    pub fn anchor(&self) -> Cursor {
        let Self { rope, state } = self;
        Cursor {
            rope,
            state: state.anchor,
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
    pub fn head(&self) -> Cursor {
        let Self { rope, state } = self;
        Cursor {
            rope,
            state: state.head,
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

    pub fn move_left(&mut self, distance: usize) -> bool {
        self.with_head(|cursor| cursor.move_left(distance))
    }

    pub fn move_right(&mut self, distance: usize) -> bool {
        self.with_head(|cursor| cursor.move_right(distance))
    }

    pub fn insert_char(&mut self, char: char) {
        self.with_head(|cursor| cursor.insert_char(char));
    }

    pub fn insert(&mut self, string: &str) {
        self.with_head(|cursor| cursor.insert(string));
    }

    pub fn backspace(&mut self, count: usize) {
        self.with_head(|cursor| cursor.backspace(count));
    }

    // TODO: Disambiguate `Delete` key and deleting the contents of the `Range`
    pub fn delete(&mut self, count: usize) {
        self.with_head(|cursor| cursor.delete(count));
    }
}
