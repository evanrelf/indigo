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
#[derive(Clone, Deserialize, Serialize)]
pub struct Edits(Vec<Edit>);

impl Edits {
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
        if let Some(last) = self.0.last_mut() {
            // There's an existing edit; try to merge
            if let Some(edit) = last.merge(edit) {
                // Merge failed; push
                self.0.push(edit);
            } else {
                // Merge succeeded
            }
        } else {
            // This is the first edit; push
            self.0.push(edit);
        }
    }
}

// TODO: Char length?
// TODO: More efficient encoding?
#[derive(Clone, Deserialize, Serialize)]
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
            // Discard no-ops
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

            // TODO: Merge more edits (e.g. truncate an insert?)

            // Merge failed, return ownership of `other`
            (_, other) => Some(other),
        }
    }
}
