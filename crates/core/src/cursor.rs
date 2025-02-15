use crate::{ot::EditSeq, rope::RopeExt as _};
use ropey::Rope;

#[derive(Clone, Copy)]
pub enum Bias {
    Before,
    After,
}

#[derive(Debug, PartialEq)]
pub struct RawCursor {
    pub index: usize, // char gap index
}

impl RawCursor {
    #[must_use]
    pub fn new(rope: &Rope, gap_index: usize, snap_bias: Bias) -> Self {
        let last_gap = rope.len_chars();

        if gap_index > last_gap {
            return Self { index: last_gap };
        }

        if let Ok(true) = rope.try_is_grapheme_boundary(gap_index) {
            return Self { index: gap_index };
        }

        let snapped = match snap_bias {
            Bias::Before => rope.get_prev_grapheme_boundary(gap_index),
            Bias::After => rope.get_next_grapheme_boundary(gap_index),
        };

        if let Ok(gap_index) = snapped {
            return Self { index: gap_index };
        }

        unreachable!()
    }

    fn snap(&mut self, rope: &Rope, snap_bias: Bias) {
        let gap_index = self.index;
        *self = Self::new(rope, gap_index, snap_bias);
    }

    pub fn move_left(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_prev_grapheme_boundary(self.index) {
                Ok(index) if self.index != index => self.index = index,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(self.index) {
                Ok(index) if self.index != index => self.index = index,
                _ => break,
            }
        }
    }

    pub fn insert_char(&mut self, rope: &mut Rope, char: char) {
        self.insert(rope, &char.to_string());
    }

    pub fn insert(&mut self, rope: &mut Rope, string: &str) {
        let mut edits = EditSeq::new();
        edits.retain(self.index);
        edits.insert(string);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.index = edits.transform_index(self.index);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        self.snap(rope, Bias::After);
    }

    pub fn backspace(&mut self, rope: &mut Rope, count: usize) {
        let mut index = None;
        for _ in 1..=count {
            match rope.get_prev_grapheme_boundary(self.index) {
                Ok(prev) if index != Some(prev) && self.index != prev => index = Some(prev),
                _ => break,
            }
        }
        if let Some(index) = index {
            let mut edits = EditSeq::new();
            edits.retain(index);
            edits.delete(self.index - index);
            edits.retain_rest(rope);
            edits.apply(rope).unwrap();
            self.index = edits.transform_index(self.index);
        }
    }
}

pub struct Cursor<'a> {
    rope: &'a Rope,
    cursor: RawCursor,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope, gap_index: usize, snap_bias: Bias) -> Self {
        let cursor = RawCursor::new(rope, gap_index, snap_bias);
        Self { rope, cursor }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn index(&self) -> usize {
        self.cursor.index
    }

    pub fn move_left(&mut self, distance: usize) {
        self.cursor.move_left(self.rope, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.cursor.move_right(self.rope, distance);
    }
}

pub struct CursorMut<'a> {
    rope: &'a mut Rope,
    cursor: RawCursor,
}

impl<'a> CursorMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope, gap_index: usize, snap_bias: Bias) -> Self {
        let cursor = RawCursor::new(rope, gap_index, snap_bias);
        Self { rope, cursor }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn index(&self) -> usize {
        self.cursor.index
    }

    pub fn move_left(&mut self, distance: usize) {
        self.cursor.move_left(self.rope, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.cursor.move_right(self.rope, distance);
    }

    pub fn insert_char(&mut self, char: char) {
        self.cursor.insert_char(self.rope, char);
    }

    pub fn insert(&mut self, string: &str) {
        self.cursor.insert(self.rope, string);
    }

    pub fn backspace(&mut self, count: usize) {
        self.cursor.backspace(self.rope, count);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

    #[test]
    fn snapping() {
        let rope = Rope::new();
        assert_eq!(RawCursor::new(&rope, 42, Bias::Before).index, 0);
        assert_eq!(RawCursor::new(&rope, 42, Bias::After).index, 0);
        let rope = Rope::from_str("👨🏻‍❤️‍💋‍👨🏻");
        assert_eq!(RawCursor::new(&rope, 0, Bias::Before).index, 0);
        assert_eq!(RawCursor::new(&rope, 10, Bias::Before).index, 10);
        assert_eq!(RawCursor::new(&rope, 1, Bias::Before).index, 0);
        assert_eq!(RawCursor::new(&rope, 9, Bias::Before).index, 0);
        assert_eq!(RawCursor::new(&rope, 0, Bias::After).index, 0);
        assert_eq!(RawCursor::new(&rope, 10, Bias::After).index, 10);
        assert_eq!(RawCursor::new(&rope, 1, Bias::After).index, 10);
        assert_eq!(RawCursor::new(&rope, 9, Bias::After).index, 10);
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut cursor = CursorMut::new(&mut rope, 0, Bias::Before);
        cursor.insert("e");
        let index = cursor.index();
        assert!(
            cursor.rope().try_is_grapheme_boundary(index).ok() == Some(true),
            "cursor not on grapheme boundary\nrope = {rope:?}\nindex = {index}"
        );
    }

    #[test]
    #[ignore]
    fn fuzz() {
        arbtest(|u| {
            let mut rope = Rope::new();
            let gap_index = u.arbitrary()?;
            let snap_bias = u.choose(&[Bias::Before, Bias::After])?;
            let mut cursor = CursorMut::new(&mut rope, gap_index, *snap_bias);
            let mut actions = Vec::new();
            for _ in 0..u.choose_index(32)? {
                match u.choose_index(4)? {
                    0 => {
                        let arg = u.choose_index(99)?;
                        cursor.move_left(arg);
                        actions.push(format!("move_left({arg})"));
                    }
                    1 => {
                        let arg = u.choose_index(99)?;
                        cursor.move_right(arg);
                        actions.push(format!("move_right({arg})"));
                    }
                    2 => {
                        let arg = u.arbitrary()?;
                        cursor.insert(arg);
                        actions.push(format!("insert({arg})"));
                    }
                    3 => {
                        let arg = u.choose_index(99)?;
                        cursor.backspace(arg);
                        actions.push(format!("backspace({arg})"));
                    }
                    _ => break,
                }
                let index = cursor.index();
                let length = cursor.rope.len_chars();
                let is_grapheme_boundary =
                    cursor.rope().try_is_grapheme_boundary(index).ok() == Some(true);
                let is_eof = index == length;
                assert!(
                    is_grapheme_boundary || is_eof,
                    "not a grapheme boundary\nactions = {actions:?}\nindex = {index}\nrope = {rope:?}\nlength = {length}"
                );
            }
            Ok(())
        });
    }
}
