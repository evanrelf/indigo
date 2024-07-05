use crate::cursor::CursorState;
use ropey::Rope;

#[derive(Clone, Copy, Default)]
pub struct RangeState {
    anchor: CursorState,
    head: CursorState,
}

#[derive(Clone, Copy)]
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
}

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
