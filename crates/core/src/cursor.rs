#![allow(clippy::trivially_copy_pass_by_ref)]

use crate::{
    ot::EditSeq,
    rope::{RopeExt as _, SnapBias},
};
use ropey::Rope;
use std::{
    borrow::{Borrow, BorrowMut},
    num::NonZeroUsize,
};

#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Cursor<T> {
    text: T,
    char_offset: usize,
}

impl Cursor<()> {
    #[must_use]
    pub fn new(char_offset: usize) -> Self {
        Self {
            text: (),
            char_offset,
        }
    }

    pub fn set_char_offset(&mut self, char_offset: usize) {
        self.char_offset = char_offset;
    }
}

impl<T> Cursor<T> {
    #[must_use]
    pub fn with<U>(self, text: U, snap_bias: SnapBias) -> Cursor<U>
    where
        U: Borrow<Rope>,
    {
        let cursor = Cursor {
            char_offset: text
                .borrow()
                .snap_to_grapheme_boundary(self.char_offset, snap_bias),
            text,
        };
        cursor.assert_valid();
        cursor
    }

    #[must_use]
    pub fn try_with<U>(self, text: U) -> Option<Cursor<U>>
    where
        U: Borrow<Rope>,
    {
        if !text.borrow().is_grapheme_boundary(self.char_offset) {
            return None;
        }
        let cursor = Cursor {
            text,
            char_offset: self.char_offset,
        };
        cursor.assert_valid();
        Some(cursor)
    }

    #[must_use]
    pub fn without(self) -> Cursor<()> {
        Cursor {
            text: (),
            char_offset: self.char_offset,
        }
    }

    #[must_use]
    pub fn char_offset(&self) -> usize {
        self.char_offset
    }
}

impl<T> Cursor<T>
where
    T: Borrow<Rope>,
{
    #[must_use]
    fn text(&self) -> &Rope {
        self.text.borrow()
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.char_offset == self.text().len_chars()
    }

    pub fn move_left(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.text().prev_grapheme_boundary(self.char_offset) {
                Some(prev) if self.char_offset != prev => self.char_offset = prev,
                _ => break,
            }
        }
    }

    pub fn move_right(&mut self, count: NonZeroUsize) {
        for _ in 1..=count.get() {
            match self.text().next_grapheme_boundary(self.char_offset) {
                Some(next) if self.char_offset != next => self.char_offset = next,
                _ => break,
            }
        }
    }

    pub(crate) fn assert_valid(&self) {
        assert!(
            self.char_offset <= self.text().len_chars(),
            "Cursor beyond end of text (char_offset={})",
            self.char_offset
        );
        assert!(
            self.text().is_grapheme_boundary(self.char_offset),
            "Cursor not on a grapheme boundary (char_offset={})",
            self.char_offset
        );
    }
}

impl<T> Cursor<T>
where
    T: BorrowMut<Rope>,
{
    #[must_use]
    fn text_mut(&mut self) -> &mut Rope {
        self.text.borrow_mut()
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
        edits.retain(self.char_offset);
        edits.insert(string);
        edits.retain_rest(self.text());
        edits.apply(self.text_mut()).unwrap();
        self.char_offset = edits.transform_char_offset(self.char_offset);
        // If the inserted string combines with existing text, the cursor would be left in the
        // middle of a new grapheme, so we must snap after inserting.
        // Makes `insert_changes_grapheme_boundary` test pass.
        // TODO: Eliminate need for explicit snapping, or reduce repetition of snapping in cursor
        // and range code.
        self.char_offset = self.text().ceil_grapheme_boundary(self.char_offset);
        edits
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let _ = self.delete_before_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_before_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut char_offset = self.char_offset;
        for _ in 1..=count.get() {
            match self.text().prev_grapheme_boundary(char_offset) {
                Some(prev) if char_offset != prev => char_offset = prev,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(char_offset);
        edits.delete(self.char_offset - char_offset);
        edits.retain_rest(self.text());
        edits.apply(self.text_mut()).unwrap();
        self.char_offset = edits.transform_char_offset(self.char_offset);
        edits
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let _ = self.delete_after_impl(count);
    }

    #[must_use]
    pub(crate) fn delete_after_impl(&mut self, count: NonZeroUsize) -> EditSeq {
        self.assert_valid();
        let mut char_offset = self.char_offset;
        for _ in 1..=count.get() {
            match self.text().next_grapheme_boundary(char_offset) {
                Some(next) if char_offset != next => char_offset = next,
                _ => break,
            }
        }
        let mut edits = EditSeq::new();
        edits.retain(self.char_offset);
        edits.delete(char_offset - self.char_offset);
        edits.retain_rest(self.text());
        edits.apply(self.text_mut()).unwrap();
        self.char_offset = edits.transform_char_offset(self.char_offset);
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
        let mut text = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut cursor = Cursor::new(0).try_with(&mut text).unwrap();
        cursor.insert("e");
        cursor.assert_valid();
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let mut text = Rope::new();
            let char_offset = u.arbitrary()?;
            let snap_bias = u.choose(&[SnapBias::Before, SnapBias::After])?;
            let mut cursor = Cursor::new(char_offset).with(&mut text, *snap_bias);
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
