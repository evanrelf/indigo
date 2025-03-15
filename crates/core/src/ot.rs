use ropey::Rope;
use std::{ops::Deref, rc::Rc};
use thiserror::Error;

// TODO: Add backtraces once this issue is resolved: https://github.com/dtolnay/thiserror/issues/390
#[derive(Debug, Error)]
pub enum Error {
    #[error("length mismatch: {left} != {right}")]
    LengthMismatch { left: usize, right: usize },

    #[error("char offset {char_offset} exceeds rope length {len_chars}")]
    CharOffsetPastEof {
        char_offset: usize,
        len_chars: usize,
    },

    #[error(transparent)]
    RopeyError(#[from] ropey::Error),
}

// TODO: More efficient encoding?
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct EditSeq {
    edits: Vec<Edit>,
    source_chars: usize,
    target_chars: usize,
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
            source_chars: 0,
            target_chars: 0,
        }
    }

    pub fn retain(&mut self, char_length: usize) {
        if char_length == 0 {
            return;
        }

        self.source_chars += char_length;
        self.target_chars += char_length;

        if let Some(Edit::Retain(n)) = self.edits.last_mut() {
            *n += char_length;
        } else {
            self.edits.push(Edit::Retain(char_length));
        }
    }

    pub fn retain_rest(&mut self, rope: &Rope) {
        // TODO: Return custom error if `source_chars` exceeds rope length.
        let char_length = rope.len_chars() - self.source_chars;

        self.retain(char_length);
    }

    pub fn delete(&mut self, char_length: usize) {
        if char_length == 0 {
            return;
        }

        self.source_chars += char_length;

        if let Some(Edit::Delete(n)) = self.edits.last_mut() {
            *n += char_length;
        } else {
            self.edits.push(Edit::Delete(char_length));
        }
    }

    pub fn insert(&mut self, text: impl Into<Rc<str>>) {
        let text = text.into();

        if text.is_empty() {
            return;
        }

        self.target_chars += text.chars().count();

        if let Some(Edit::Insert(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.chars().count() + text.chars().count());
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
        if self.target_chars != other.source_chars {
            return Err(Error::LengthMismatch {
                left: self.target_chars,
                right: other.source_chars,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L172
        // https://github.com/helix-editor/helix/blob/ba4793fca0f8198ce57bf6bc1d46ed7649665b33/helix-core/src/transaction.rs#L153

        todo!()
    }

    pub fn transform(&self, other: &Self) -> Result<(Self, Self), Error> {
        if self.source_chars != other.source_chars {
            return Err(Error::LengthMismatch {
                left: self.source_chars,
                right: other.source_chars,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L344

        todo!()
    }

    #[must_use]
    pub fn transform_char_offset(&self, mut char_offset: usize) -> usize {
        let mut position = 0;

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => {
                    position += n;
                }
                Edit::Delete(n) => {
                    if position < char_offset {
                        char_offset -= n;
                    }
                }
                Edit::Insert(s) => {
                    if position <= char_offset {
                        char_offset += s.chars().count();
                    }
                    position += s.chars().count();
                }
            }
        }

        char_offset
    }

    pub fn invert(&self, rope: &Rope) -> Result<Self, Error> {
        let length_mismatch = || Error::LengthMismatch {
            left: self.source_chars,
            right: rope.len_chars(),
        };

        if self.source_chars != rope.len_chars() {
            return Err(length_mismatch());
        }

        let mut inverted = Self::with_capacity(self.len());

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => inverted.retain(*n),
                Edit::Delete(n) => {
                    let start = inverted.target_chars;
                    let end = start + n;
                    let s = rope
                        .get_slice(start..end)
                        .ok_or_else(length_mismatch)?
                        .to_string();
                    inverted.insert(s);
                }
                Edit::Insert(s) => inverted.delete(s.chars().count()),
            }
        }

        Ok(inverted)
    }

    pub fn apply(&self, rope: &mut Rope) -> Result<(), Error> {
        if self.source_chars != rope.len_chars() {
            return Err(Error::LengthMismatch {
                left: self.source_chars,
                right: rope.len_chars(),
            });
        }

        let mut char_offset = 0;

        for edit in &self.edits {
            char_offset = edit.apply(char_offset, rope)?;
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
    pub fn apply(&self, char_offset: usize, rope: &mut Rope) -> Result<usize, Error> {
        let char_offset = match self {
            Self::Retain(n) => char_offset + n,
            Self::Delete(n) => {
                rope.try_remove(char_offset..char_offset + n)?;
                char_offset
            }
            Self::Insert(s) => {
                rope.try_insert(char_offset, s)?;
                char_offset + s.chars().count()
            }
        };

        if char_offset > rope.len_chars() {
            return Err(Error::CharOffsetPastEof {
                char_offset,
                len_chars: rope.len_chars(),
            });
        }

        Ok(char_offset)
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
    fn edits_transform_offset() {
        let mut rope = Rope::from("Hello, world!");

        let mut char_offset = 9;
        assert_eq!(rope.char(char_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("Hello, ".chars().count());
        edits.retain("world".chars().count());
        edits.insert("!!!");
        edits.retain("!".chars().count());

        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("world!!!!"));

        assert_eq!(rope.get_char(char_offset), None);
        char_offset = edits.transform_char_offset(char_offset);
        assert_eq!(char_offset, 2);
        assert_eq!(rope.char(char_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("w".chars().count());
        edits.insert("the whole w");
        edits.retain_rest(&rope);
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("the whole world!!!!"));

        assert_eq!(rope.char(char_offset), 'e');
        char_offset = edits.transform_char_offset(char_offset);
        assert_eq!(char_offset, 12);
        assert_eq!(rope.char(char_offset), 'r');
    }

    #[test]
    fn edits_invert() {
        let rope1 = Rope::from("Hello, world!");

        let mut edits = EditSeq::new();
        edits.delete("H".chars().count());
        edits.insert("Y");
        edits.retain("ello".chars().count());
        edits.insert("w");
        edits.delete(", world!".chars().count());
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
        edits.delete("world".chars().count());
        edits.insert("Evan");
        edits.delete("!".chars().count());
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
        let char_offset = 0;
        let char_offset = Edit::Retain(7).apply(char_offset, &mut rope).unwrap();
        let char_offset = Edit::Delete("world".chars().count())
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("Evan"))
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Delete("!".chars().count())
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("..."))
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("?"))
            .apply(char_offset, &mut rope)
            .unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::Retain(1).apply(char_offset, &mut rope).is_err());
    }

    // Given a string S and consecutive operations A and B, the following must hold:
    //
    // ```
    // apply(apply(S, A), B) = apply(S, compose(A, B))
    // ```
    // #[test]
    // fn prop() {
    //     arbtest(|u| {
    //         let s = u.arbitrary::<Rope>()?;
    //         let a = u.arbitrary::<EditSeq>()?;
    //         let b = u.arbitrary::<EditSeq>()?;
    //         assert_eq!(); // TODO
    //     });
    // }
}
