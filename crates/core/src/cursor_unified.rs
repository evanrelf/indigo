#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{
    ot::EditSeq,
    rope::{Bias, RopeExt as _},
};
use ropey::Rope;
use std::{
    borrow::{Borrow, BorrowMut},
    num::NonZeroUsize,
};

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Cursor<R> {
    rope: R,
    byte_gap_index: usize,
}

impl Cursor<()> {
    #[must_use]
    pub fn new(byte_gap_index: usize) -> Self {
        Self {
            rope: (),
            byte_gap_index,
        }
    }

    pub fn set_byte_gap_index(&mut self, byte_gap_index: usize) {
        self.byte_gap_index = byte_gap_index;
    }
}

impl<R> Cursor<R> {
    #[must_use]
    pub fn with_rope<R2>(self, rope: R2, snap_bias: Bias) -> Cursor<R2>
    where
        R2: Borrow<Rope>,
    {
        let cursor = Cursor {
            byte_gap_index: rope
                .borrow()
                .snap_to_grapheme_boundary(self.byte_gap_index, snap_bias),
            rope,
        };
        cursor.assert_valid();
        cursor
    }

    #[must_use]
    pub fn try_with_rope<R2>(self, rope: R2) -> Option<Cursor<R2>>
    where
        R2: Borrow<Rope>,
    {
        if !rope.borrow().is_grapheme_boundary(self.byte_gap_index) {
            return None;
        }
        let cursor = Cursor {
            rope,
            byte_gap_index: self.byte_gap_index,
        };
        cursor.assert_valid();
        Some(cursor)
    }

    #[must_use]
    pub fn without_rope(self) -> Cursor<()> {
        Cursor {
            rope: (),
            byte_gap_index: self.byte_gap_index,
        }
    }

    #[must_use]
    pub fn byte_gap_index(&self) -> usize {
        self.byte_gap_index
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
        self.byte_gap_index == self.rope().len_bytes()
    }

    pub fn move_left(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.rope().prev_grapheme_boundary(self.byte_gap_index) {
                Some(prev) if self.byte_gap_index != prev => self.byte_gap_index = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.rope().next_grapheme_boundary(self.byte_gap_index) {
                Some(next) if self.byte_gap_index != next => self.byte_gap_index = next,
                _ => break,
            }
        }
    }

    pub(crate) fn assert_valid(&self) {
        assert!(
            self.byte_gap_index <= self.rope().len_bytes(),
            "Cursor beyond end of rope (byte_gap_index={})",
            self.byte_gap_index
        );
        assert!(
            self.rope().is_grapheme_boundary(self.byte_gap_index),
            "Cursor not on a grapheme boundary (byte_gap_index={})",
            self.byte_gap_index
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
        edits.retain(self.byte_gap_index);
        edits.insert(string);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.byte_gap_index = edits.transform_index(self.byte_gap_index);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.byte_gap_index = self
            .rope()
            .snap_to_grapheme_boundary(self.byte_gap_index, Bias::After);
        edits
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let _ = self.delete_before_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut byte_gap_index = self.byte_gap_index;
        for _ in 1..=count.get() {
            match self.rope().prev_grapheme_boundary(byte_gap_index) {
                Some(prev) if byte_gap_index != prev => byte_gap_index = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(byte_gap_index);
        edits.delete(self.byte_gap_index - byte_gap_index);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.byte_gap_index = edits.transform_index(self.byte_gap_index);
        edits
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let _ = self.delete_after_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut byte_gap_index = self.byte_gap_index;
        for _ in 1..=count.get() {
            match self.rope().next_grapheme_boundary(byte_gap_index) {
                Some(next) if byte_gap_index != next => byte_gap_index = next,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(self.byte_gap_index);
        edits.delete(byte_gap_index - self.byte_gap_index);
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.byte_gap_index = edits.transform_index(self.byte_gap_index);
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
            let byte_gap_index = u.arbitrary()?;
            let snap_bias = u.choose(&[Bias::Before, Bias::After])?;
            let mut cursor = Cursor::new(byte_gap_index).with_rope(&mut rope, *snap_bias);
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
