use ropey::Rope;
use std::{ops::Deref, rc::Rc};
use thiserror::Error;

// TODO: Add backtraces once this issue is resolved: https://github.com/dtolnay/thiserror/issues/390
#[derive(Debug, Error)]
pub enum Error {
    #[error("length mismatch: {left} != {right}")]
    LengthMismatch { left: usize, right: usize },

    #[error("byte index {byte_index} exceeds rope length {len_bytes}")]
    ByteIndexPastEof { byte_index: usize, len_bytes: usize },

    #[error(transparent)]
    RopeyError(#[from] ropey::Error),
}

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Eq, PartialEq)]
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
            self.edits.push(Edit::Retain(byte_length));
        }
    }

    pub fn retain_rest(&mut self, rope: &Rope) {
        // TODO: Return custom error if `source_bytes` exceeds rope length.
        let byte_length = rope.len_bytes() - self.source_bytes;

        self.retain(byte_length);
    }

    pub fn delete(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.source_bytes += byte_length;

        if let Some(Edit::Delete(n)) = self.edits.last_mut() {
            *n += byte_length;
        } else {
            self.edits.push(Edit::Delete(byte_length));
        }
    }

    pub fn insert(&mut self, text: impl Into<Rc<str>>) {
        let text = text.into();

        if text.is_empty() {
            return;
        }

        self.target_bytes += text.len();

        if let Some(Edit::Insert(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(&text);
            *last = Rc::from(s);
        } else {
            self.edits.push(Edit::Insert(text));
        }
    }

    pub fn push(&mut self, edit: Edit) {
        match edit {
            Edit::Retain(n) => self.retain(n),
            Edit::Delete(n) => self.delete(n),
            Edit::Insert(s) => self.insert(s),
        }
    }

    pub fn compose(&self, other: &Self) -> Result<Self, Error> {
        if self.target_bytes != other.source_bytes {
            return Err(Error::LengthMismatch {
                left: self.target_bytes,
                right: other.source_bytes,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L172
        // https://github.com/helix-editor/helix/blob/ba4793fca0f8198ce57bf6bc1d46ed7649665b33/helix-core/src/transaction.rs#L153

        todo!()
    }

    pub fn transform(&self, other: &Self) -> Result<(Self, Self), Error> {
        if self.source_bytes != other.source_bytes {
            return Err(Error::LengthMismatch {
                left: self.source_bytes,
                right: other.source_bytes,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L344

        todo!()
    }

    #[must_use]
    pub fn transform_index(&self, mut byte_index: usize) -> usize {
        let mut position = 0;

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => {
                    position += n;
                }
                Edit::Delete(n) => {
                    if position < byte_index {
                        byte_index -= n;
                    }
                }
                Edit::Insert(s) => {
                    if position <= byte_index {
                        byte_index += s.len();
                    }
                    position += s.len();
                }
            }
        }

        byte_index
    }

    pub fn invert(&self, rope: &Rope) -> Result<Self, Error> {
        let length_mismatch = || Error::LengthMismatch {
            left: self.source_bytes,
            right: rope.len_bytes(),
        };

        if self.source_bytes != rope.len_bytes() {
            return Err(length_mismatch());
        }

        let mut inverted = Self::with_capacity(self.len());

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => inverted.retain(*n),
                Edit::Delete(n) => {
                    let start = inverted.target_bytes;
                    let end = start + n;
                    let s = rope
                        .get_slice(start..end)
                        .ok_or_else(length_mismatch)?
                        .to_string();
                    inverted.insert(s);
                }
                Edit::Insert(s) => inverted.delete(s.len()),
            }
        }

        Ok(inverted)
    }

    pub fn apply(&self, rope: &mut Rope) -> Result<(), Error> {
        if self.source_bytes != rope.len_bytes() {
            return Err(Error::LengthMismatch {
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Edit {
    Retain(usize),
    Delete(usize),
    Insert(Rc<str>),
}

impl Edit {
    pub fn apply(&self, byte_index: usize, rope: &mut Rope) -> Result<usize, Error> {
        let byte_index = match self {
            Self::Retain(n) => byte_index + n,
            Self::Delete(n) => {
                rope.try_remove(byte_index..byte_index + n)?;
                byte_index
            }
            Self::Insert(s) => {
                rope.try_insert(byte_index, s)?;
                byte_index + s.len()
            }
        };

        if byte_index > rope.len_bytes() {
            return Err(Error::ByteIndexPastEof {
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
            Edit::Insert(Rc::from("Hello,")),
            Edit::Insert(Rc::from(" world!")),
            Edit::Retain(42),
            Edit::Retain(58),
            Edit::Delete(4),
            Edit::Delete(4),
            Edit::Delete(4),
        ]);
        assert_eq!(
            *edits,
            [
                Edit::Insert(Rc::from("Hello, world!")),
                Edit::Retain(100),
                Edit::Delete(12),
            ]
        );
    }

    #[test]
    fn edits_transform_index() {
        let mut rope = Rope::from("Hello, world!");

        let mut index = 9;
        assert_eq!(rope.char(index), 'r');

        let mut edits = EditSeq::new();
        edits.delete("Hello, ".len());
        edits.retain("world".len());
        edits.insert("!!!");
        edits.retain("!".len());

        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("world!!!!"));

        assert_eq!(rope.get_char(index), None);
        index = edits.transform_index(index);
        assert_eq!(index, 2);
        assert_eq!(rope.char(index), 'r');

        let mut edits = EditSeq::new();
        edits.delete("w".len());
        edits.insert("the whole w");
        edits.retain_rest(&rope);
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("the whole world!!!!"));

        assert_eq!(rope.char(index), 'e');
        index = edits.transform_index(index);
        assert_eq!(index, 12);
        assert_eq!(rope.char(index), 'r');
    }

    #[test]
    fn edits_invert() {
        let rope1 = Rope::from("Hello, world!");

        let mut edits = EditSeq::new();
        edits.delete("H".len());
        edits.insert("Y");
        edits.retain("ello".len());
        edits.insert("w");
        edits.delete(", world!".len());
        edits.insert(" and pink");

        let mut rope2 = rope1.clone();
        edits.apply(&mut rope2).unwrap();
        assert_eq!(rope2, Rope::from("Yellow and pink"));

        assert_eq!(edits.invert(&rope1).unwrap().invert(&rope2).unwrap(), edits);

        let mut rope3 = rope2.clone();
        edits.invert(&rope1).unwrap().apply(&mut rope3).unwrap();
        assert_eq!(rope3, Rope::from("Hello, world!"));
    }

    #[test]
    fn edits_apply() {
        let mut edits = EditSeq::new();
        edits.retain(7);
        edits.delete("world".len());
        edits.insert("Evan");
        edits.delete("!".len());
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
        let byte_index = Edit::Retain(7).apply(byte_index, &mut rope).unwrap();
        let byte_index = Edit::Delete("world".len())
            .apply(byte_index, &mut rope)
            .unwrap();
        let byte_index = Edit::Insert(Rc::from("Evan"))
            .apply(byte_index, &mut rope)
            .unwrap();
        let byte_index = Edit::Delete("!".len())
            .apply(byte_index, &mut rope)
            .unwrap();
        let byte_index = Edit::Insert(Rc::from("..."))
            .apply(byte_index, &mut rope)
            .unwrap();
        let byte_index = Edit::Insert(Rc::from("?"))
            .apply(byte_index, &mut rope)
            .unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::Retain(1).apply(byte_index, &mut rope).is_err());
    }

    // TODO: Write property test for this with `quickcheck` or `proptest`.
    //
    // Given a string S and consecutive operations A and B, the following must hold:
    //
    // ```
    // apply(apply(S, A), B) = apply(S, compose(A, B))
    // ```
}
