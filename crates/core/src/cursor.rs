#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{ot::EditSeq, rope::RopeExt as _};
use ropey::Rope;

#[derive(Clone, Copy)]
pub enum Bias {
    Before,
    After,
}

// Fields should be public because there are no invariants to enforce. Correctness is only
// meaningful relative to a rope, which this type does not control.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct RawCursor {
    pub gap_index: usize, // char gap index
}

impl RawCursor {
    #[must_use]
    pub fn new(rope: &Rope, gap_index: usize, snap_bias: Bias) -> Self {
        Self {
            gap_index: snap(rope, gap_index, snap_bias),
        }
    }

    #[must_use]
    pub fn is_eof(&self, rope: &Rope) -> bool {
        self.gap_index == rope.len_chars()
    }

    pub fn move_left(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_prev_grapheme_boundary(self.gap_index) {
                Ok(prev) if self.gap_index != prev => self.gap_index = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(self.gap_index) {
                Ok(next) if self.gap_index != next => self.gap_index = next,
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
        self.gap_index = snap(rope, self.gap_index, Bias::After);
        edits
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: usize) {
        let _ = self.delete_before_impl(rope, count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, rope: &mut Rope, count: usize) -> EditSeq {
        let mut gap_index = self.gap_index;
        for _ in 1..=count {
            match rope.get_prev_grapheme_boundary(gap_index) {
                Ok(prev) if gap_index != prev => gap_index = prev,
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
        let mut gap_index = self.gap_index;
        for _ in 1..=count {
            match rope.get_next_grapheme_boundary(gap_index) {
                Ok(next) if gap_index != next => gap_index = next,
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

    pub(crate) fn is_valid(&self, rope: &Rope) -> bool {
        rope.try_is_grapheme_boundary(self.gap_index).ok() == Some(true)
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
    pub fn gap_index(&self) -> usize {
        self.cursor.gap_index
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.rope)
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
    pub fn gap_index(&self) -> usize {
        self.cursor.gap_index
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.cursor.is_eof(self.rope)
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

    pub fn delete_after(&mut self, count: usize) {
        self.cursor.delete_after(self.rope, count);
    }

    pub(crate) fn is_valid(&self) -> bool {
        self.cursor.is_valid(self.rope)
    }
}

/// Snap character gap index to next grapheme boundary in given direction.
// TODO: Move to `rope` module and give a better name?
#[must_use]
pub fn snap(rope: &Rope, gap_index: usize, bias: Bias) -> usize {
    let last_gap = rope.len_chars();

    if gap_index > last_gap {
        return last_gap;
    }

    if let Ok(true) = rope.try_is_grapheme_boundary(gap_index) {
        return gap_index;
    }

    let snapped = match bias {
        Bias::Before => rope.get_prev_grapheme_boundary(gap_index),
        Bias::After => rope.get_next_grapheme_boundary(gap_index),
    };

    if let Ok(gap_index) = snapped {
        return gap_index;
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

    #[test]
    fn test_snap() {
        let rope = Rope::new();
        assert_eq!(snap(&rope, 42, Bias::Before), 0);
        assert_eq!(snap(&rope, 42, Bias::After), 0);
        let rope = Rope::from_str("👨🏻‍❤️‍💋‍👨🏻");
        assert_eq!(snap(&rope, 0, Bias::Before), 0);
        assert_eq!(snap(&rope, 10, Bias::Before), 10);
        assert_eq!(snap(&rope, 1, Bias::Before), 0);
        assert_eq!(snap(&rope, 9, Bias::Before), 0);
        assert_eq!(snap(&rope, 0, Bias::After), 0);
        assert_eq!(snap(&rope, 10, Bias::After), 10);
        assert_eq!(snap(&rope, 1, Bias::After), 10);
        assert_eq!(snap(&rope, 9, Bias::After), 10);
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut cursor = CursorMut::new(&mut rope, 0, Bias::Before);
        cursor.insert("e");
        let gap_index = cursor.gap_index();
        assert!(
            cursor.is_valid(),
            "cursor not on grapheme boundary\nrope = {rope:?}\ngap_index = {gap_index}"
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
                let gap_index = cursor.gap_index();
                let length = cursor.rope.len_chars();
                assert!(
                    cursor.is_valid(),
                    "not a grapheme boundary\nactions =\n  {actions}\ngap_index = {gap_index}\nrope = {rope:?}\nlength = {length}"
                );
            }
            Ok(())
        });
    }
}
