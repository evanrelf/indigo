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
    fn anchor_mut(&mut self) -> CursorMut {
        let Self { rope, state } = self;
        CursorMut {
            rope,
            state: state.anchor,
        }
    }

    #[must_use]
    fn head_mut(&mut self) -> CursorMut {
        let Self { rope, state } = self;
        CursorMut {
            rope,
            state: state.head,
        }
    }

    fn with_anchor_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let Self { rope, state } = self;
        let mut anchor = CursorMut {
            rope,
            state: state.anchor,
        };
        let result = func(&mut anchor);
        state.anchor = anchor.state;
        result
    }

    fn with_head_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let Self { rope, state } = self;
        let mut head = CursorMut {
            rope,
            state: state.head,
        };
        let result = func(&mut head);
        state.head = head.state;
        result
    }

    pub fn insert_char(&mut self, char: char) {
        self.with_head_mut(|cursor| cursor.insert_char(char));
    }

    pub fn insert(&mut self, string: &str) {
        self.with_head_mut(|cursor| cursor.insert(string));
    }

    pub fn backspace(&mut self, count: usize) {
        self.with_head_mut(|cursor| cursor.backspace(count));
    }

    // TODO: Disambiguate `Delete` key and deleting the contents of the `Range`
    pub fn delete(&mut self, count: usize) {
        self.with_head_mut(|cursor| cursor.delete(count));
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

    fn with_anchor<T>(&mut self, func: impl Fn(&mut Cursor) -> T) -> T {
        let (rope, state) = self.range_parts_mut();
        let mut anchor = Cursor {
            rope,
            state: state.anchor,
        };
        let result = func(&mut anchor);
        state.anchor = anchor.state;
        result
    }

    fn with_head<T>(&mut self, func: impl Fn(&mut Cursor) -> T) -> T {
        let (rope, state) = self.range_parts_mut();
        let mut head = Cursor {
            rope,
            state: state.head,
        };
        let result = func(&mut head);
        state.head = head.state;
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

impl RangeExt for Range<'_> {
    fn range_parts(&self) -> (&Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}

impl RangeExt for RangeMut<'_> {
    fn range_parts(&self) -> (&Rope, &RangeState) {
        (self.rope, &self.state)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RangeState) {
        (self.rope, &mut self.state)
    }
}
