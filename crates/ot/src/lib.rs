// TODO: Remove
#![allow(unused_variables)]

use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::{ops::Deref, rc::Rc};
use thiserror::Error;

// TODO: Make `Edit::Delete` use a number instead of a string

#[derive(Debug, Error)]
pub enum EditError {
    #[error("length mismatch: {left} != {right}")]
    LengthMismatch { left: usize, right: usize },

    #[error("byte index {byte_index} in the middle of a multi-byte char with index {char_index}")]
    ByteIndexMisaligned {
        byte_index: usize,
        char_index: usize,
    },

    #[error("byte index {byte_index} exceeds rope length {len_bytes}")]
    ByteIndexPastEof { byte_index: usize, len_bytes: usize },

    #[error(transparent)]
    RopeyError(#[from] ropey::Error),
}

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct EditSeq {
    edits: Vec<Edit>,
    source_bytes: usize,
    target_bytes: usize,
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
            source_bytes: 0,
            target_bytes: 0,
        }
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.source_bytes += byte_length;
        self.target_bytes += byte_length;

        if let Some(Edit::Retain(n)) = self.edits.last_mut() {
            *n += byte_length;
        } else {
            self.edits.push(Edit::retain(byte_length));
        }
    }

    pub fn delete(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.source_bytes += text.len();

        if let Some(Edit::Delete(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            let _ = std::mem::replace(last, Rc::from(s));
        } else {
            self.edits.push(Edit::delete(text));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.target_bytes += text.len();

        if let Some(Edit::Insert(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            let _ = std::mem::replace(last, Rc::from(s));
        } else {
            self.edits.push(Edit::insert(text));
        }
    }

    pub fn push(&mut self, edit: Edit) {
        match edit {
            Edit::Retain(n) => self.retain(n),
            Edit::Delete(s) => self.delete(&s),
            Edit::Insert(s) => self.insert(&s),
        }
    }

    pub fn compose(&self, other: &Self) -> Result<Self, EditError> {
        if self.target_bytes != other.source_bytes {
            return Err(EditError::LengthMismatch {
                left: self.target_bytes,
                right: other.source_bytes,
            });
        }

        todo!()
    }

    pub fn transform(&self, other: &Self) -> Result<(Self, Self), EditError> {
        if self.source_bytes != other.source_bytes {
            return Err(EditError::LengthMismatch {
                left: self.source_bytes,
                right: other.source_bytes,
            });
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

    pub fn apply(&self, rope: &mut Rope) -> Result<(), EditError> {
        if self.source_bytes != rope.len_bytes() {
            return Err(EditError::LengthMismatch {
                left: self.source_bytes,
                right: rope.len_bytes(),
            });
        }

        let mut byte_index = 0;

        for edit in &self.edits {
            byte_index = edit.apply(byte_index, rope)?;
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
    pub fn retain(byte_length: usize) -> Self {
        Self::Retain(byte_length)
    }

    #[must_use]
    pub fn delete(text: &str) -> Self {
        Self::Delete(Rc::from(text))
    }

    #[must_use]
    pub fn insert(text: &str) -> Self {
        Self::Insert(Rc::from(text))
    }

    pub fn apply(&self, byte_index: usize, rope: &mut Rope) -> Result<usize, EditError> {
        let char_index = rope.try_byte_to_char(byte_index)?;

        if byte_index != rope.char_to_byte(char_index) {
            return Err(EditError::ByteIndexMisaligned {
                byte_index,
                char_index,
            });
        }

        let byte_index = match self {
            Self::Retain(n) => byte_index + n,
            Self::Delete(s) => {
                rope.try_remove(char_index..char_index + s.len())?;
                byte_index
            }
            Self::Insert(s) => {
                rope.try_insert(char_index, s)?;
                byte_index + s.len()
            }
        };

        if byte_index > rope.len_bytes() {
            return Err(EditError::ByteIndexPastEof {
                byte_index,
                len_bytes: rope.len_bytes(),
            });
        }

        Ok(byte_index)
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
        edits.retain("ello".len());
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
    fn edit_apply() {
        let mut rope = Rope::from("Hello, world!");
        let byte_index = 0;
        let byte_index = Edit::retain(7).apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::delete("world").apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::insert("Evan").apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::delete("!").apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::insert("...").apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::insert("?").apply(byte_index, &mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::retain(1).apply(byte_index, &mut rope).is_err());
    }
}
