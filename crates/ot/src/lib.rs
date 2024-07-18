use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::rc::Rc;

/*
pub trait Operation: Sized {
    type Error;

    fn compose(&self, other: &Self) -> Result<Self, Self::Error>;

    fn transform(&self, other: &Self) -> Result<(Self, Self), Self::Error>;

    fn apply(&self, target: ()) -> Result<(), Self::Error>;

    fn invert(&self, target: ()) -> Self;

    // #[must_use]
    // fn parent(&self) -> usize;
}
*/

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Edits {
    edits: Vec<Edit>,
    // TODO: Track input (rope this can apply to) and output (length of rope after edits) lengths?
}

impl Edits {
    #[must_use]
    pub fn new() -> Self {
        Self { edits: Vec::new() }
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            edits: Vec::with_capacity(capacity),
        }
    }

    pub fn retain(&mut self, length: usize) {
        self.push(Edit::retain(length));
    }

    pub fn delete(&mut self, length: usize) {
        self.push(Edit::delete(length));
    }

    pub fn insert(&mut self, text: &str) {
        self.push(Edit::insert(text));
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

    pub fn apply(&self, rope: &Rope) -> anyhow::Result<Rope> {
        todo!()
    }

    #[must_use]
    pub fn invert(&self, rope: &Rope) -> Self {
        todo!()
    }
}

// TODO: Char length?
// TODO: More efficient encoding?
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Edit {
    Retain(usize),
    Delete(usize),
    Insert(Rc<str>),
}

impl Edit {
    #[must_use]
    pub fn retain(length: usize) -> Self {
        Self::Retain(length)
    }

    #[must_use]
    pub fn delete(length: usize) -> Self {
        Self::Delete(length)
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
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Write tests for `Edits`

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
}
