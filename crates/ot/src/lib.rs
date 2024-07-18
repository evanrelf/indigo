use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::{ops::Deref, rc::Rc};

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct EditSeq {
    edits: Vec<Edit>,
    // TODO: Track input (rope this can apply to) and output (length of rope after edits) lengths?
}

impl EditSeq {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn empty() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            edits: Vec::with_capacity(capacity),
        }
    }

    pub fn retain(&mut self, char_length: usize) -> &mut Self {
        self.push(Edit::retain(char_length));
        self
    }

    pub fn delete(&mut self, char_length: usize) -> &mut Self {
        self.push(Edit::delete(char_length));
        self
    }

    pub fn insert(&mut self, text: &str) -> &mut Self {
        self.push(Edit::insert(text));
        self
    }

    pub fn push(&mut self, edit: Edit) {
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
        todo!()
    }

    pub fn transform(&self, other: &Self) -> anyhow::Result<(Self, Self)> {
        todo!()
    }

    #[must_use]
    pub fn invert(&self, rope: &Rope) -> Self {
        todo!()
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
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
    Delete(usize),
    Insert(Rc<str>),
}

impl Edit {
    #[must_use]
    pub fn retain(char_length: usize) -> Self {
        Self::Retain(char_length)
    }

    #[must_use]
    pub fn delete(char_length: usize) -> Self {
        Self::Delete(char_length)
    }

    #[must_use]
    pub fn insert(text: &str) -> Self {
        Self::Insert(Rc::from(text))
    }

    pub fn merge(&mut self, other: Self) -> Option<Self> {
        use Edit::{Delete, Insert, Retain};

        match (self, other) {
            // Discard empty edits
            (_, Retain(0) | Delete(0)) => None,
            (_, Insert(r)) if r.is_empty() => None,
            (Insert(l), Insert(r)) if l.is_empty() => {
                let _ = std::mem::replace(l, r);
                None
            }

            (Retain(l), Retain(r)) | (Delete(l), Delete(r)) => {
                *l += r;
                None
            }

            (Insert(l), Insert(r)) => {
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
            Self::Delete(n) => {
                rope.try_remove(char_index..char_index + n)?;
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
        let mut edits = EditSeq::empty();
        edits.extend([
            Edit::insert("Hello,"),
            Edit::insert(" world!"),
            Edit::retain(42),
            Edit::retain(58),
            Edit::delete(100),
        ]);
        assert_eq!(
            *edits,
            [
                Edit::insert("Hello, world!"),
                Edit::retain(100),
                Edit::delete(100)
            ]
        );
    }

    #[test]
    fn edits_apply() {
        let mut edits = EditSeq::empty();
        edits.retain(7);
        edits.delete(5);
        edits.insert("Evan");
        edits.delete(1);
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
        assert_eq!(e1, Edit::Retain(42));

        let mut e1 = Edit::retain(0);
        let e2 = Edit::retain(42);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Retain(42));

        // Delete 0
        let mut e1 = Edit::delete(42);
        let e2 = Edit::delete(0);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Delete(42));

        let mut e1 = Edit::delete(0);
        let e2 = Edit::delete(42);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Delete(42));

        // Insert ""
        let mut e1 = Edit::insert("hello");
        let e2 = Edit::insert("");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Insert(Rc::from("hello")));

        let mut e1 = Edit::insert("");
        let e2 = Edit::insert("hello");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Insert(Rc::from("hello")));
    }

    #[test]
    fn edit_merge_grow() {
        // Retain
        let mut e1 = Edit::retain(25);
        let e2 = Edit::retain(50);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Retain(75));

        // Delete
        let mut e1 = Edit::delete(25);
        let e2 = Edit::delete(50);
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Delete(75));

        // Insert
        let mut e1 = Edit::insert("hello");
        let e2 = Edit::insert(" world");
        assert_eq!(e1.merge(e2), None);
        assert_eq!(e1, Edit::Insert(Rc::from("hello world")));
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
        let char_index = Edit::delete(5).apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("Evan").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::delete(1).apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("...").apply(char_index, &mut rope).unwrap();
        let char_index = Edit::insert("?").apply(char_index, &mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::retain(1).apply(char_index, &mut rope).is_err());
    }
}
