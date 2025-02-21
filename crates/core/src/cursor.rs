#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{
    ot::EditSeq,
    rope::{Bias, RopeExt as _},
};
use ropey::Rope;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct RawCursor {
    pub gap_index: usize, // char gap index
}

impl RawCursor {
    #[must_use]
    pub fn new(rope: &Rope, gap_index: usize) -> Option<Self> {
        if rope.is_grapheme_boundary(gap_index) {
            Some(Self { gap_index })
        } else {
            None
        }
    }

    #[must_use]
    pub fn new_snapped(rope: &Rope, gap_index: usize, snap_bias: Bias) -> Self {
        Self {
            gap_index: rope.snap_to_grapheme_boundary(gap_index, snap_bias),
        }
    }

    #[must_use]
    pub fn is_eof(&self, rope: &Rope) -> bool {
        self.gap_index == rope.len_chars()
    }

    pub fn move_left(&mut self, rope: &Rope, count: usize) {
        for _ in 1..=count {
            match rope.prev_grapheme_boundary(self.gap_index) {
                Some(prev) if self.gap_index != prev => self.gap_index = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, rope: &Rope, count: usize) {
        for _ in 1..=count {
            match rope.next_grapheme_boundary(self.gap_index) {
                Some(next) if self.gap_index != next => self.gap_index = next,
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
        self.assert_valid(rope);
        let mut edits = EditSeq::new();
        edits.retain(self.gap_index);
        edits.insert(string);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.gap_index = rope.snap_to_grapheme_boundary(self.gap_index, Bias::After);
        edits
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: usize) {
        let _ = self.delete_before_impl(rope, count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, rope: &mut Rope, count: usize) -> EditSeq {
        self.assert_valid(rope);
        let mut gap_index = self.gap_index;
        for _ in 1..=count {
            match rope.prev_grapheme_boundary(gap_index) {
                Some(prev) if gap_index != prev => gap_index = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(gap_index);
        edits.delete(self.gap_index - gap_index);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        edits
    }

    pub fn delete_after(&mut self, rope: &mut Rope, count: usize) {
        let _ = self.delete_after_impl(rope, count);
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self, rope: &mut Rope, count: usize) -> EditSeq {
        self.assert_valid(rope);
        let mut gap_index = self.gap_index;
        for _ in 1..=count {
            match rope.next_grapheme_boundary(gap_index) {
                Some(next) if gap_index != next => gap_index = next,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(self.gap_index);
        edits.delete(gap_index - self.gap_index);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        edits
    }

    pub(crate) fn assert_valid(&self, rope: &Rope) {
        assert!(
            self.gap_index <= rope.len_chars(),
            "Cursor beyond end of rope (gap_index={})",
            self.gap_index
        );
        assert!(
            rope.is_grapheme_boundary(self.gap_index),
            "Cursor not on a grapheme boundary (gap_index={})",
            self.gap_index
        );
    }
}

pub struct Cursor<'a> {
    rope: &'a Rope,
    cursor: RawCursor,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope, gap_index: usize) -> Option<Self> {
        let cursor = RawCursor::new(rope, gap_index)?;
        Some(Self { rope, cursor })
    }

    #[must_use]
    pub fn new_snapped(rope: &'a Rope, gap_index: usize, snap_bias: Bias) -> Self {
        let cursor = RawCursor::new_snapped(rope, gap_index, snap_bias);
        Self { rope, cursor }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn gap_index(&self) -> usize {
        self.cursor.gap_index
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.rope)
    }

    pub fn move_left(&mut self, count: usize) {
        self.cursor.move_left(self.rope, count);
    }

    pub fn move_right(&mut self, count: usize) {
        self.cursor.move_right(self.rope, count);
    }

    pub(crate) fn assert_valid(&self) {
        self.cursor.assert_valid(self.rope);
    }
}

pub struct CursorMut<'a> {
    rope: &'a mut Rope,
    cursor: RawCursor,
}

impl<'a> CursorMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope, gap_index: usize) -> Option<Self> {
        let cursor = RawCursor::new(rope, gap_index)?;
        Some(Self { rope, cursor })
    }

    #[must_use]
    pub fn new_snapped(rope: &'a mut Rope, gap_index: usize, snap_bias: Bias) -> Self {
        let cursor = RawCursor::new_snapped(rope, gap_index, snap_bias);
        Self { rope, cursor }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn gap_index(&self) -> usize {
        self.cursor.gap_index
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.rope)
    }

    pub fn move_left(&mut self, count: usize) {
        self.cursor.move_left(self.rope, count);
    }

    pub fn move_right(&mut self, count: usize) {
        self.cursor.move_right(self.rope, count);
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

    pub fn delete_after(&mut self, count: usize) {
        self.cursor.delete_after(self.rope, count);
    }

    pub(crate) fn assert_valid(&self) {
        self.cursor.assert_valid(self.rope);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut cursor = CursorMut::new(&mut rope, 0).unwrap();
        cursor.insert("e");
        cursor.assert_valid();
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let mut rope = Rope::new();
            let gap_index = u.arbitrary()?;
            let snap_bias = u.choose(&[Bias::Before, Bias::After])?;
            let mut cursor = CursorMut::new_snapped(&mut rope, gap_index, *snap_bias);
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
                let _ = actions;
                cursor.assert_valid();
            }
            Ok(())
        });
    }
}
