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

    pub(crate) fn snap(&mut self, rope: &Rope, snap_bias: Bias) {
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
        let _ = self.insert_impl(rope, string);
    }

    #[must_use]
    pub(crate) fn insert_impl(&mut self, rope: &mut Rope, string: &str) -> EditSeq {
        let mut edits = EditSeq::new();
        edits.retain(self.index);
        edits.insert(string);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.index = edits.transform_index(self.index);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.snap(rope, Bias::After);
        edits
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: usize) {
        let _ = self.delete_before_impl(rope, count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, rope: &mut Rope, count: usize) -> EditSeq {
        let mut index = self.index;
        for _ in 1..=count {
            match rope.get_prev_grapheme_boundary(index) {
                Ok(prev) if index != prev => index = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(index);
        edits.delete(self.index - index);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.index = edits.transform_index(self.index);
        edits
    }

    pub(crate) fn is_valid(&self, rope: &Rope) -> bool {
        rope.try_is_grapheme_boundary(self.index).ok() == Some(true)
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

    pub(crate) fn is_valid(&self) -> bool {
        self.cursor.is_valid(self.rope)
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

    pub fn delete_before(&mut self, count: usize) {
        self.cursor.delete_before(self.rope, count);
    }

    pub(crate) fn is_valid(&self) -> bool {
        self.cursor.is_valid(self.rope)
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
            cursor.is_valid(),
            "cursor not on grapheme boundary\nrope = {rope:?}\nindex = {index}"
        );
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let mut rope = Rope::new();
            let gap_index = u.arbitrary()?;
            let snap_bias = u.choose(&[Bias::Before, Bias::After])?;
            let mut cursor = CursorMut::new(&mut rope, gap_index, *snap_bias);
            let mut actions = Vec::new();
            for _ in 0..u.choose_index(100)? {
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
                        // let arg = u.arbitrary()?; // TODO: Pass test with arbitrary Unicode
                        let arg = u.choose(&["", "x", "\t", "\n", "🇯🇵", "👨‍👨‍👧"])?;
                        cursor.insert(arg);
                        actions.push(format!("insert({arg:?})"));
                    }
                    3 => {
                        let arg = u.choose_index(99)?;
                        cursor.delete_before(arg);
                        actions.push(format!("delete_before({arg})"));
                    }
                    _ => break,
                }
                let actions = actions.join("\n  ");
                let index = cursor.index();
                let length = cursor.rope.len_chars();
                assert!(
                    cursor.is_valid(),
                    "not a grapheme boundary\nactions =\n  {actions}\nindex = {index}\nrope = {rope:?}\nlength = {length}"
                );
            }
            Ok(())
        });
    }
}
