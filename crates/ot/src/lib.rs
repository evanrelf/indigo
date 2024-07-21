// TODO: Remove
#![allow(unused_variables)]

use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::{ops::Deref, rc::Rc};

// TODO: Use `thiserror` instead of `anyhow` for errors
// TODO: Move merge logic out of `Edit` and into `EditSeq::{retain,delete,insert}`
// TODO: Make `EditSeq::push` call `EditSeq::{retain,delete,insert}`
// TODO: Use byte lengths instead of char lengths for greater efficiency
//       (`string.len()` is O(1), `string.chars().count()` is O(n))
// TODO: Make `Edit::Delete` use a number instead of a string

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct EditSeq {
    edits: Vec<Edit>,
    source_length: usize,
    target_length: usize,
}

impl EditSeq {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            edits: Vec::with_capacity(capacity),
            source_length: 0,
            target_length: 0,
        }
    }

    pub fn retain(&mut self, char_length: usize) {
        self.push(Edit::retain(char_length));
    }

    pub fn delete(&mut self, text: &str) {
        self.push(Edit::delete(text));
    }

    pub fn insert(&mut self, text: &str) {
        self.push(Edit::insert(text));
    }

    pub fn push(&mut self, edit: Edit) {
        match edit {
            Edit::Retain(n) => {
                self.source_length += n;
                self.target_length += n;
            }
            Edit::Delete(ref s) => {
                self.source_length += s.chars().count();
            }
            Edit::Insert(ref s) => {
                self.target_length += s.chars().count();
            }
        }

        // Check if there's an existing edit to merge with
        if let Some(last) = self.edits.last_mut() {
            // There's an existing edit; try to merge
            if let Some(edit) = last.merge(edit) {
                // Merge failed; push
                self.edits.push(edit);
            } else {
                // Merge succeeded
            }
        } else {
            // This is the first edit; push
            self.edits.push(edit);
        }
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        if self.target_length != other.source_length {
            anyhow::bail!("Left target length doesn't match right source length");
        }

        todo!()
    }

    pub fn transform(&self, other: &Self) -> anyhow::Result<(Self, Self)> {
        if self.source_length != other.source_length {
            anyhow::bail!("Left source length doesn't match right source length");
        }

        todo!()
    }

    pub fn invert(&mut self) {
        for edit in &mut self.edits {
            match edit {
                Edit::Retain(_) => {}
                Edit::Delete(s) => *edit = Edit::Insert(s.clone()),
                Edit::Insert(s) => *edit = Edit::Delete(s.clone()),
            }
        }
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        if self.source_length != rope.len_chars() {
            anyhow::bail!("Source length doesn't match rope length");
        }

        let mut char_index = 0;

        for edit in &self.edits {
            char_index = edit.apply(char_index, rope)?;
        }

        Ok(())
    }
}

impl Deref for EditSeq {
    type Target = [Edit];

    fn deref(&self) -> &Self::Target {
        &self.edits
    }
}

impl IntoIterator for EditSeq {
    type Item = Edit;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.into_iter()
    }
}

impl<'a> IntoIterator for &'a EditSeq {
    type Item = &'a Edit;

    type IntoIter = std::slice::Iter<'a, Edit>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.iter()
    }
}

impl Extend<Edit> for EditSeq {
    fn extend<T>(&mut self, edits: T)
    where
        T: IntoIterator<Item = Edit>,
    {
        for edit in edits {
            self.push(edit);
        }
    }
}

// TODO: More efficient encoding?
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Edit {
    Retain(usize),
    // TODO: Check that deleted text matches in rope.
    // TODO: Store `usize` rather than `Rc<str>` for `Delete`, recover string from rope when
    // inverting? Gain efficiency at the cost of added complexity.
    Delete(Rc<str>),
    Insert(Rc<str>),
}

impl Edit {
    #[must_use]
    pub fn retain(char_length: usize) -> Self {
        Self::Retain(char_length)
    }

    #[must_use]
    pub fn delete(text: &str) -> Self {
        Self::Delete(Rc::from(text))
    }

    #[must_use]
    pub fn insert(text: &str) -> Self {
        Self::Insert(Rc::from(text))
    }

    pub fn merge(&mut self, other: Self) -> Option<Self> {
        use Edit::{Delete, Insert, Retain};

        match (self, other) {
            // Discard empty edits
            (_, Retain(0)) => None,
            (_, Delete(r) | Insert(r)) if r.is_empty() => None,
            (Insert(l), Insert(r)) | (Delete(l), Delete(r)) if l.is_empty() => {
                let _ = std::mem::replace(l, r);
                None
            }

            (Retain(l), Retain(r)) => {
                *l += r;
                None
            }

            (Insert(l), Insert(r)) | (Delete(l), Delete(r)) => {
                let mut s = String::with_capacity(l.len() + r.len());
                s.push_str(l);
                s.push_str(&r);
                let _ = std::mem::replace(l, Rc::from(s));
                None
            }

            // TODO: Merge more edits (e.g. cancel out delete "x" and insert "x")

            // Merge failed, return ownership of `other`
            (_, other) => Some(other),
        }
    }

    pub fn apply(&self, char_index: usize, rope: &mut Rope) -> anyhow::Result<usize> {
        let char_index = match self {
            Self::Retain(n) => char_index + n,
            Self::Delete(s) => {
                rope.try_remove(char_index..char_index + s.chars().count())?;
                char_index
            }
            Self::Insert(s) => {
                rope.try_insert(char_index, s)?;
                char_index + s.chars().count()
            }
        };
        anyhow::ensure!(
            char_index <= rope.len_chars(),
            "char_index exceeds end of rope"
        );
        Ok(char_index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Write more tests for `EditSeq`

    #[test]
    fn edit_seq_push() {
        let mut edits = EditSeq::new();
        edits.extend([
            Edit::insert("Hello,"),
            Edit::insert(" world!"),
            Edit::retain(42),
            Edit::retain(58),
            Edit::delete(" omg"),
            Edit::delete(" wtf"),
            Edit::delete(" bbq"),
        ]);
        assert_eq!(
            *edits,
            [
                Edit::insert("Hello, world!"),
                Edit::retain(100),
                Edit::delete(" omg wtf bbq"),
            ]
        );
    }

    #[test]
    fn edits_invert() {
        let mut edits = EditSeq::new();
        edits.delete("H");
        edits.insert("Y");
        edits.retain("ello".chars().count());
        edits.insert("w");
        edits.delete(", world!");
        edits.insert(" and pink");
        let mut inverted = edits.clone();
        inverted.invert();
        inverted.invert();
        assert_eq!(edits, inverted);
    }

    #[test]
    fn edits_apply() {
        let mut edits = EditSeq::new();
        edits.retain(7);
        edits.delete("world");
        edits.insert("Evan");
        edits.delete("!");
        edits.insert("...");
        edits.insert("?");

        let mut rope = Rope::from("Hello, world!");
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));

        let mut rope = Rope::from("Hello, world!");
        edits.retain(1);
        assert!(edits.apply(&mut rope).is_err());
    }

    #[test]
    fn edit_merge_empty() {
        // Retain 0
        let mut e1 = Edit::retain(42);
        let e2 = Edit::retain(0);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::retain(42));

        let mut e1 = Edit::retain(0);
        let e2 = Edit::retain(42);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::retain(42));

        // Delete 0
        let mut e1 = Edit::delete("foo");
        let e2 = Edit::delete("");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::delete("foo"));

        let mut e1 = Edit::delete("");
        let e2 = Edit::delete("foo");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::delete("foo"));

        // Insert ""
        let mut e1 = Edit::insert("hello");
        let e2 = Edit::insert("");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::insert("hello"));

        let mut e1 = Edit::insert("");
        let e2 = Edit::insert("hello");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::insert("hello"));
    }

    #[test]
    fn edit_merge_grow() {
        // Retain
        let mut e1 = Edit::retain(25);
        let e2 = Edit::retain(50);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::retain(75));

        // Delete
        let mut e1 = Edit::delete("foo");
        let e2 = Edit::delete("bar");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::delete("foobar"));

        // Insert
        let mut e1 = Edit::insert("hello");
        let e2 = Edit::insert(" world");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::insert("hello world"));
    }

    #[test]
    fn edit_merge_shrink() {
        // TODO: Add merging logic that shrinks edits
    }

    #[test]
    fn edit_apply() {
        let mut rope = Rope::from("Hello, world!");
        let char_index = 0;
        let char_index = Edit::retain(7).apply(char_index, &mut rope).unwrap();
        let char_index = Edit::delete("world").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("Evan").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::delete("!").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("...").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("?").apply(char_index, &mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::retain(1).apply(char_index, &mut rope).is_err());
    }
}
