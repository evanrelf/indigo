// TODO: Remove
#![allow(unused_variables)]

use crate::Direction;
use ropey::Rope;

// TODO: Change from `char_index` representation to `line` + `offset` representation. Otherwise you
// can't do an after insert (`a`/`A`) at the end of a line, only at the end of the rope.

#[derive(Clone, Copy)]
pub struct WeakPosition {
    pub char_index: usize,
}

impl WeakPosition {
    #[must_use]
    pub fn new(char_index: usize) -> Self {
        Self { char_index }
    }

    #[must_use]
    pub fn upgrade(self, rope: &Rope) -> Option<Position<'_>> {
        Position::new(rope, self.char_index)
    }

    #[must_use]
    pub fn upgrade_mut(self, rope: &mut Rope) -> Option<PositionMut<'_>> {
        PositionMut::new(rope, self.char_index)
    }
}

impl From<Position<'_>> for WeakPosition {
    fn from(position: Position<'_>) -> Self {
        position.downgrade()
    }
}

impl From<PositionMut<'_>> for WeakPosition {
    fn from(position_mut: PositionMut<'_>) -> Self {
        position_mut.downgrade()
    }
}

pub struct Position<'a> {
    rope: &'a Rope,
    char_index: usize,
}

impl Position<'_> {
    #[must_use]
    pub fn new(rope: &Rope, char_index: usize) -> Option<Position<'_>> {
        if char_index <= rope.len_chars() {
            Some(Position { rope, char_index })
        } else {
            None
        }
    }

    #[must_use]
    pub fn char_index(&self) -> usize {
        self.char_index
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakPosition {
        WeakPosition::new(self.char_index)
    }

    pub fn move_up(&mut self, distance: usize) {
        self.char_index =
            move_vertically(self.rope, self.char_index, Direction::Backward, distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        self.char_index = move_vertically(self.rope, self.char_index, Direction::Forward, distance);
    }

    pub fn move_left(&mut self, distance: usize) {
        self.char_index =
            move_horizontally(self.rope, self.char_index, Direction::Backward, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.char_index =
            move_horizontally(self.rope, self.char_index, Direction::Forward, distance);
    }
}

impl<'a> From<PositionMut<'a>> for Position<'a> {
    fn from(position_mut: PositionMut<'a>) -> Position<'a> {
        Position {
            rope: position_mut.rope,
            char_index: position_mut.char_index,
        }
    }
}

pub struct PositionMut<'a> {
    rope: &'a mut Rope,
    char_index: usize,
}

impl PositionMut<'_> {
    pub fn new(rope: &mut Rope, char_index: usize) -> Option<PositionMut<'_>> {
        if char_index <= rope.len_chars() {
            Some(PositionMut { rope, char_index })
        } else {
            None
        }
    }

    #[must_use]
    pub fn char_index(&self) -> usize {
        self.char_index
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakPosition {
        WeakPosition {
            char_index: self.char_index,
        }
    }

    pub fn move_up(&mut self, distance: usize) {
        self.char_index =
            move_vertically(self.rope, self.char_index, Direction::Backward, distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        self.char_index = move_vertically(self.rope, self.char_index, Direction::Forward, distance);
    }

    pub fn move_left(&mut self, distance: usize) {
        self.char_index =
            move_horizontally(self.rope, self.char_index, Direction::Backward, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.char_index =
            move_horizontally(self.rope, self.char_index, Direction::Forward, distance);
    }

    pub fn insert_before(&mut self, text: &str) {
        todo!()
    }

    pub fn insert_after(&mut self, text: &str) {
        if self.char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }

    pub fn delete_before(&mut self, count: usize) {
        if self.char_index == 0 {
            return;
        }

        todo!()
    }

    pub fn delete(&mut self) {
        if self.char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }

    pub fn delete_after(&mut self, count: usize) {
        if self.char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }
}

#[must_use]
fn move_horizontally(
    rope: &Rope,
    char_index: usize,
    direction: Direction,
    grapheme_distance: usize,
) -> usize {
    todo!()
}

#[must_use]
fn move_vertically(
    rope: &Rope,
    char_index: usize,
    direction: Direction,
    grapheme_distance: usize,
) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
}
