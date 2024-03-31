// TODO: Remove
#![allow(clippy::diverging_sub_expression)]
#![allow(unreachable_code)]
#![allow(unused_variables)]

use crate::Direction;
use ropey::Rope;

#[derive(Clone, Copy)]
pub struct WeakPosition {
    pub line: usize,
    pub char_offset: usize,
}

impl WeakPosition {
    #[must_use]
    pub fn new(line: usize, char_offset: usize) -> Self {
        Self { line, char_offset }
    }

    #[must_use]
    pub fn upgrade(self, rope: &Rope) -> Option<Position<'_>> {
        Position::new(rope, self.line, self.char_offset)
    }

    #[must_use]
    pub fn upgrade_mut(self, rope: &mut Rope) -> Option<PositionMut<'_>> {
        PositionMut::new(rope, self.line, self.char_offset)
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
    line: usize,
    char_offset: usize,
}

impl Position<'_> {
    #[must_use]
    pub fn new(rope: &Rope, line: usize, char_offset: usize) -> Option<Position<'_>> {
        if line >= rope.len_lines() {
            return None;
        }

        if char_offset >= rope.line(line).len_chars() {
            return None;
        }

        Some(Position {
            rope,
            line,
            char_offset,
        })
    }

    #[must_use]
    pub fn char_index(&self) -> Option<usize> {
        to_char_index(self.rope, self.downgrade())
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakPosition {
        WeakPosition::new(self.line, self.char_offset)
    }

    pub fn move_up(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_down(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_left(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_right(&mut self, distance: usize) {
        todo!()
    }
}

impl<'a> From<PositionMut<'a>> for Position<'a> {
    fn from(position_mut: PositionMut<'a>) -> Position<'a> {
        Position {
            rope: position_mut.rope,
            line: position_mut.line,
            char_offset: position_mut.char_offset,
        }
    }
}

pub struct PositionMut<'a> {
    rope: &'a mut Rope,
    line: usize,
    char_offset: usize,
}

impl PositionMut<'_> {
    pub fn new(rope: &mut Rope, line: usize, char_offset: usize) -> Option<PositionMut<'_>> {
        if line >= rope.len_lines() {
            return None;
        }

        if char_offset >= rope.line(line).len_chars() {
            return None;
        }

        Some(PositionMut {
            rope,
            line,
            char_offset,
        })
    }

    #[must_use]
    pub fn char_index(&self) -> Option<usize> {
        to_char_index(self.rope, self.downgrade())
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakPosition {
        WeakPosition::new(self.line, self.char_offset)
    }

    pub fn move_up(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_down(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_left(&mut self, distance: usize) {
        todo!()
    }

    pub fn move_right(&mut self, distance: usize) {
        todo!()
    }

    pub fn insert_before(&mut self, text: &str) {
        todo!()
    }

    pub fn insert_after(&mut self, text: &str) {
        let char_index: usize = todo!();

        if char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }

    pub fn delete_before(&mut self, count: usize) {
        let char_index: usize = todo!();

        if char_index == 0 {
            return;
        }

        todo!()
    }

    pub fn delete(&mut self) {
        let char_index: usize = todo!();

        if char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }

    pub fn delete_after(&mut self, count: usize) {
        let char_index: usize = todo!();

        if char_index == self.rope.len_chars() {
            return;
        }

        todo!()
    }
}

#[must_use]
fn to_char_index(rope: &Rope, weak_position: WeakPosition) -> Option<usize> {
    todo!()
}

#[must_use]
fn from_char_index(rope: &Rope, char_index: usize) -> Option<WeakPosition> {
    todo!()
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
    line_distance: usize,
) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        assert!(Position::new(&Rope::from(""), 0, 0).is_none());
        assert!(Position::new(&Rope::from("x"), 0, 0).is_some());
        assert!(Position::new(&Rope::from("x"), 0, 1).is_none());
        assert!(Position::new(&Rope::from("x\nx\n"), 1, 1).is_some());
        assert!(Position::new(&Rope::from("x\nx\n"), 1, 2).is_none());
        assert!(PositionMut::new(&mut Rope::from(""), 0, 0).is_none());
        assert!(PositionMut::new(&mut Rope::from("x"), 0, 0).is_some());
        assert!(PositionMut::new(&mut Rope::from("x"), 0, 1).is_none());
        assert!(PositionMut::new(&mut Rope::from("x\nx\n"), 1, 1).is_some());
        assert!(PositionMut::new(&mut Rope::from("x\nx\n"), 1, 2).is_none());
    }
}
