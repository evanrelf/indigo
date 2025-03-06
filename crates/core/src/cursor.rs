#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{ot::EditSeq, rope::RopeExt as _, unicode::SnapBias};
use ropey::Rope;
use std::{
    borrow::{Borrow, BorrowMut},
    num::NonZeroUsize,
};

#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Cursor<R> {
    rope: R,
    gap_index: usize,
}

impl Cursor<()> {
    #[must_use]
    pub fn new(gap_index: usize) -> Self {
        Self {
            rope: (),
            gap_index,
        }
    }

    pub fn set_gap_index(&mut self, gap_index: usize) {
        self.gap_index = gap_index;
    }
}

impl<T> Cursor<T> {
    #[must_use]
    pub fn with_rope<R>(self, rope: R, snap_bias: SnapBias) -> Cursor<R>
    where
        R: Borrow<Rope>,
    {
        let cursor = Cursor {
            gap_index: rope
                .borrow()
                .snap_to_grapheme_boundary(self.gap_index, snap_bias),
            rope,
        };
        cursor.assert_valid();
        cursor
    }

    #[must_use]
    pub fn try_with_rope<R>(self, rope: R) -> Option<Cursor<R>>
    where
        R: Borrow<Rope>,
    {
        if !rope.borrow().is_grapheme_boundary(self.gap_index) {
            return None;
        }
        let cursor = Cursor {
            rope,
            gap_index: self.gap_index,
        };
        cursor.assert_valid();
        Some(cursor)
    }

    #[must_use]
    pub fn without_rope(self) -> Cursor<()> {
        Cursor {
            rope: (),
            gap_index: self.gap_index,
        }
    }

    #[must_use]
    pub fn gap_index(&self) -> usize {
        self.gap_index
    }
}

impl<R> Cursor<R>
where
    R: Borrow<Rope>,
{
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope.borrow()
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.gap_index == self.rope().len_chars()
    }

    pub fn move_left(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.rope().prev_grapheme_boundary(self.gap_index) {
                Some(prev) if self.gap_index != prev => self.gap_index = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.rope().next_grapheme_boundary(self.gap_index) {
                Some(next) if self.gap_index != next => self.gap_index = next,
                _ => break,
            }
        }
    }

    pub(crate) fn assert_valid(&self) {
        assert!(
            self.gap_index <= self.rope().len_chars(),
            "Cursor beyond end of rope (gap_index={})",
            self.gap_index
        );
        assert!(
            self.rope().is_grapheme_boundary(self.gap_index),
            "Cursor not on a grapheme boundary (gap_index={})",
            self.gap_index
        );
    }
}

impl<R> Cursor<R>
where
    R: BorrowMut<Rope>,
{
    #[must_use]
    fn rope_mut(&mut self) -> &mut Rope {
        self.rope.borrow_mut()
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, string: &str) {
        let _ = self.insert_impl(string);
    }

    #[must_use]
    pub(crate) fn insert_impl(&mut self, string: &str) -> EditSeq {
        self.assert_valid();
        let mut edits = EditSeq::new();
        edits.retain(self.gap_index);
        edits.insert(string);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.gap_index = self
            .rope()
            .snap_to_grapheme_boundary(self.gap_index, SnapBias::After);
        edits
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let _ = self.delete_before_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut gap_index = self.gap_index;
        for _ in 1..=count.get() {
            match self.rope().prev_grapheme_boundary(gap_index) {
                Some(prev) if gap_index != prev => gap_index = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(gap_index);
        edits.delete(self.gap_index - gap_index);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        edits
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let _ = self.delete_after_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut gap_index = self.gap_index;
        for _ in 1..=count.get() {
            match self.rope().next_grapheme_boundary(gap_index) {
                Some(next) if gap_index != next => gap_index = next,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(self.gap_index);
        edits.delete(gap_index - self.gap_index);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.gap_index = edits.transform_index(self.gap_index);
        edits
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;
    use std::cmp::max;

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut cursor = Cursor::new(0).try_with_rope(&mut rope).unwrap();
        cursor.insert("e");
        cursor.assert_valid();
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let mut rope = Rope::new();
            let gap_index = u.arbitrary()?;
            let snap_bias = u.choose(&[SnapBias::Before, SnapBias::After])?;
            let mut cursor = Cursor::new(gap_index).with_rope(&mut rope, *snap_bias);
            let mut actions = Vec::new();
            for _ in 0..u.choose_index(100)? {
                match u.choose_index(4)? {
                    0 => {
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
                        cursor.move_left(arg);
                        actions.push(format!("move_left({arg})"));
                    }
                    1 => {
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
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
                        let arg = NonZeroUsize::new(max(1, u.choose_index(99)?)).unwrap();
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
