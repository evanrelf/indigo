//! Operational transformations.
//!
//! <https://en.wikipedia.org/wiki/Operational_transformation>

use ropey::Rope;
use std::{
    cmp::min,
    ops::{Deref, Range},
    rc::Rc,
};
use thiserror::Error;

pub trait Edit {
    type Insertion;
    type Deletion;
    type Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error>;
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error>;
}

pub struct Ot {
    pub rope: Rope,
    pub ot: Vec<EditSeq>,
}

pub struct OtInsertion {
    pub byte_offset: usize,
    pub text: String,
}

pub struct OtDeletion {
    pub range: Range<usize>,
}

impl Edit for Ot {
    type Insertion = OtInsertion;
    type Deletion = OtDeletion;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error> {
        let mut edits = EditSeq::new();
        edits.retain(offset);
        edits.insert(text);
        edits.retain_rest(&self.rope);
        edits.apply(&mut self.rope)?;
        self.ot.push(edits);
        Ok(OtInsertion {
            byte_offset: offset,
            text: String::from(text),
        })
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error> {
        let mut edits = EditSeq::new();
        edits.retain(range.start);
        edits.delete(range.end - range.start);
        edits.retain_rest(&self.rope);
        edits.apply(&mut self.rope)?;
        self.ot.push(edits);
        Ok(OtDeletion { range })
    }
}

pub struct Crdt {
    pub rope: Rope,
    pub crdt: cola::Replica,
}

pub struct CrdtInsertion {
    pub text: String,
    pub crdt: cola::Insertion,
}

pub struct CrdtDeletion {
    pub crdt: cola::Deletion,
}

impl Edit for Crdt {
    type Insertion = CrdtInsertion;
    type Deletion = CrdtDeletion;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error> {
        self.rope.insert(offset, text);
        let insertion = self.crdt.inserted(offset, text.len());
        Ok(CrdtInsertion {
            text: String::from(text),
            crdt: insertion,
        })
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error> {
        self.rope.remove(range.clone());
        let deletion = self.crdt.deleted(range);
        Ok(CrdtDeletion { crdt: deletion })
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Length mismatch: {left} != {right}")]
    LengthMismatch { left: usize, right: usize },

    #[error("Byte offset {byte_offset} exceeds rope length {len_bytes}")]
    ByteOffsetPastEnd {
        byte_offset: usize,
        len_bytes: usize,
    },

    #[error("Failed to compose: first sequence produces more bytes than second expects")]
    ComposeFirstProducesMore,

    #[error("Failed to compose: second sequence expects more bytes than first produces")]
    ComposeSecondExpectsMore,

    #[error("Error from rope")]
    Rope(#[from] ropey::Error),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct EditSeq {
    edits: Vec<EditOp>,
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

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.source_bytes += byte_length;
        self.target_bytes += byte_length;

        if let Some(EditOp::Retain(n)) = self.edits.last_mut() {
            *n += byte_length;
        } else {
            self.edits.push(EditOp::Retain(byte_length));
        }
    }

    pub fn retain_rest(&mut self, rope: &Rope) {
        // TODO: Return custom error if `source_bytes` exceeds rope length.
        let byte_length = rope.len() - self.source_bytes;

        self.retain(byte_length);
    }

    pub fn delete(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.source_bytes += byte_length;

        if let Some(EditOp::Delete(n)) = self.edits.last_mut() {
            *n += byte_length;
        } else {
            self.edits.push(EditOp::Delete(byte_length));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.target_bytes += text.len();

        if let Some(EditOp::Insert(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            *last = Rc::from(s);
        } else {
            self.edits.push(EditOp::Insert(Rc::from(text)));
        }
    }

    pub fn push(&mut self, edit: EditOp) {
        match edit {
            EditOp::Retain(n) => self.retain(n),
            EditOp::Delete(n) => self.delete(n),
            EditOp::Insert(s) => self.insert(&s),
        }
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        if self.target_bytes != other.source_bytes {
            anyhow::bail!(Error::LengthMismatch {
                left: self.target_bytes,
                right: other.source_bytes,
            });
        }

        let mut result = Self::with_capacity(self.len() + other.len());

        let mut iter1 = self.edits.iter().cloned();
        let mut iter2 = other.edits.iter().cloned();

        let mut op1 = iter1.next();
        let mut op2 = iter2.next();

        while op1.is_some() || op2.is_some() {
            match (&mut op1, &mut op2) {
                // Delete from first: pass through, doesn't consume from second
                (Some(EditOp::Delete(n)), _) => {
                    result.delete(*n);
                    op1 = iter1.next();
                }

                // Insert from second: pass through, doesn't consume from first
                (_, Some(EditOp::Insert(s))) => {
                    result.insert(s);
                    op2 = iter2.next();
                }

                // Retain + Retain: both consume, output retain
                (Some(EditOp::Retain(n1)), Some(EditOp::Retain(n2))) => {
                    let n = min(*n1, *n2);
                    result.retain(n);
                    *n1 -= n;
                    *n2 -= n;
                    if *n1 == 0 {
                        op1 = iter1.next();
                    }
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Retain + Delete: both consume, output delete
                (Some(EditOp::Retain(n1)), Some(EditOp::Delete(n2))) => {
                    let n = min(*n1, *n2);
                    result.delete(n);
                    *n1 -= n;
                    *n2 -= n;
                    if *n1 == 0 {
                        op1 = iter1.next();
                    }
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Insert + Retain: second retains inserted text
                (Some(EditOp::Insert(s1)), Some(EditOp::Retain(n2))) => {
                    let len1 = s1.len();
                    let n = len1.min(*n2);

                    if n == len1 {
                        // Second retains all of first's insert
                        result.insert(s1);
                        op1 = iter1.next();
                    } else {
                        // Second retains only part: split the insert
                        let before = Rc::from(&s1[..n]);
                        let after = Rc::from(&s1[n..]);
                        result.insert(&before);
                        *s1 = after;
                    }

                    *n2 -= n;
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Insert + Delete: operations cancel out
                (Some(EditOp::Insert(s1)), Some(EditOp::Delete(n2))) => {
                    let len1 = s1.len();
                    let n = len1.min(*n2);

                    if n == len1 {
                        // All of insert is deleted
                        op1 = iter1.next();
                    } else {
                        // Only part deleted: keep the rest
                        let after = Rc::from(&s1[n..]);
                        *s1 = after;
                    }

                    *n2 -= n;
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                (None, Some(EditOp::Retain(_) | EditOp::Delete(_))) => {
                    anyhow::bail!(Error::ComposeSecondExpectsMore);
                }

                (Some(EditOp::Retain(_) | EditOp::Insert(_)), None) => {
                    anyhow::bail!(Error::ComposeFirstProducesMore);
                }

                (None, None) => break,
            }
        }

        Ok(result)
    }

    // TODO
    #[doc(hidden)]
    pub fn transform(&self, other: &Self) -> anyhow::Result<(Self, Self)> {
        if self.source_bytes != other.source_bytes {
            anyhow::bail!(Error::LengthMismatch {
                left: self.source_bytes,
                right: other.source_bytes,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L344

        todo!()
    }

    #[must_use]
    pub fn transform_byte_offset(&self, mut byte_offset: usize) -> usize {
        let mut position = 0;

        for edit in &self.edits {
            match edit {
                EditOp::Retain(n) => {
                    position += n;
                }
                EditOp::Delete(n) => {
                    if position < byte_offset {
                        byte_offset -= n;
                    }
                }
                EditOp::Insert(s) => {
                    if position <= byte_offset {
                        byte_offset += s.len();
                    }
                    position += s.len();
                }
            }
        }

        byte_offset
    }

    pub fn invert(&self, rope: &Rope) -> anyhow::Result<Self> {
        let length_mismatch = || Error::LengthMismatch {
            left: self.source_bytes,
            right: rope.len(),
        };

        if self.source_bytes != rope.len() {
            anyhow::bail!(length_mismatch());
        }

        let mut inverted = Self::with_capacity(self.len());

        for edit in &self.edits {
            match edit {
                EditOp::Retain(n) => inverted.retain(*n),
                EditOp::Delete(n) => {
                    let start = inverted.target_bytes;
                    let end = start + n;
                    let s = rope.slice(start..end).to_string();
                    inverted.insert(&s);
                }
                EditOp::Insert(s) => inverted.delete(s.len()),
            }
        }

        Ok(inverted)
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        if self.source_bytes != rope.len() {
            anyhow::bail!(Error::LengthMismatch {
                left: self.source_bytes,
                right: rope.len(),
            });
        }

        let mut byte_offset = 0;

        for edit in &self.edits {
            byte_offset = edit.apply(byte_offset, rope)?;
        }

        Ok(())
    }
}

impl Deref for EditSeq {
    type Target = [EditOp];

    fn deref(&self) -> &Self::Target {
        &self.edits
    }
}

impl IntoIterator for EditSeq {
    type Item = EditOp;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.into_iter()
    }
}

impl<'a> IntoIterator for &'a EditSeq {
    type Item = &'a EditOp;

    type IntoIter = std::slice::Iter<'a, EditOp>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.iter()
    }
}

impl Extend<EditOp> for EditSeq {
    fn extend<T>(&mut self, edits: T)
    where
        T: IntoIterator<Item = EditOp>,
    {
        for edit in edits {
            self.push(edit);
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EditOp {
    Retain(usize),
    Delete(usize),
    Insert(Rc<str>),
}

impl EditOp {
    pub fn apply(&self, byte_offset: usize, rope: &mut Rope) -> anyhow::Result<usize> {
        let byte_offset = match self {
            Self::Retain(n) => byte_offset + n,
            Self::Delete(n) => {
                rope.try_remove(byte_offset..byte_offset + n)?;
                byte_offset
            }
            Self::Insert(s) => {
                rope.try_insert(byte_offset, s)?;
                byte_offset + s.len()
            }
        };

        if byte_offset > rope.len() {
            anyhow::bail!(Error::ByteOffsetPastEnd {
                byte_offset,
                len_bytes: rope.len(),
            });
        }

        Ok(byte_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edit_ot() -> anyhow::Result<()> {
        let mut text = Ot {
            rope: Rope::from("The quick brown fox"),
            ot: Vec::new(),
        };
        text.delete(4..9)?;
        assert_eq!(text.rope, Rope::from("The  brown fox"));
        text.insert(4, "cute")?;
        assert_eq!(text.rope, Rope::from("The cute brown fox"));
        text.delete(9..14)?;
        assert_eq!(text.rope, Rope::from("The cute  fox"));
        text.insert(9, "white")?;
        assert_eq!(text.rope, Rope::from("The cute white fox"));
        Ok(())
    }

    #[test]
    fn edit_crdt() -> anyhow::Result<()> {
        let rope = Rope::from("The quick brown fox");
        let mut text = Crdt {
            crdt: cola::Replica::new(1, rope.len()),
            rope,
        };
        text.delete(4..9)?;
        assert_eq!(text.rope, Rope::from("The  brown fox"));
        text.insert(4, "cute")?;
        assert_eq!(text.rope, Rope::from("The cute brown fox"));
        text.delete(9..14)?;
        assert_eq!(text.rope, Rope::from("The cute  fox"));
        text.insert(9, "white")?;
        assert_eq!(text.rope, Rope::from("The cute white fox"));
        Ok(())
    }

    #[test]
    fn edit_seq_push() {
        let mut edits = EditSeq::new();
        edits.extend([
            EditOp::Insert(Rc::from("Hello,")),
            EditOp::Insert(Rc::from(" world!")),
            EditOp::Retain(42),
            EditOp::Retain(58),
            EditOp::Delete(4),
            EditOp::Delete(4),
            EditOp::Delete(4),
        ]);
        assert_eq!(
            *edits,
            [
                EditOp::Insert(Rc::from("Hello, world!")),
                EditOp::Retain(100),
                EditOp::Delete(12),
            ]
        );
    }

    #[test]
    fn edits_transform_offset() {
        let mut rope = Rope::from("Hello, world!");

        let mut byte_offset = 9;
        assert_eq!(rope.char(byte_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("Hello, ".len());
        edits.retain("world".len());
        edits.insert("!!!");
        edits.retain("!".len());

        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("world!!!!"));

        assert_eq!(rope.get_char(byte_offset).ok(), None);
        byte_offset = edits.transform_byte_offset(byte_offset);
        assert_eq!(byte_offset, 2);
        assert_eq!(rope.char(byte_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("w".len());
        edits.insert("the whole w");
        edits.retain_rest(&rope);
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("the whole world!!!!"));

        assert_eq!(rope.char(byte_offset), 'e');
        byte_offset = edits.transform_byte_offset(byte_offset);
        assert_eq!(byte_offset, 12);
        assert_eq!(rope.char(byte_offset), 'r');
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
        let byte_offset = 0;
        let byte_offset = EditOp::Retain(7).apply(byte_offset, &mut rope).unwrap();
        let byte_offset = EditOp::Delete("world".len())
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = EditOp::Insert(Rc::from("Evan"))
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = EditOp::Delete("!".len())
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = EditOp::Insert(Rc::from("..."))
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = EditOp::Insert(Rc::from("?"))
            .apply(byte_offset, &mut rope)
            .unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(EditOp::Retain(1).apply(byte_offset, &mut rope).is_err());
    }

    #[test]
    fn edits_compose_basic() {
        // S0 = "abcdef"
        // A: Delete "ab", retain "cdef" -> S1 = "cdef"
        // B: Retain "cd", delete "ef" -> S2 = "cd"
        // A ∘ B should transform "abcdef" -> "cd"

        let mut edit_a = EditSeq::new();
        edit_a.delete(2); // Delete "ab"
        edit_a.retain(4); // Retain "cdef"

        let mut edit_b = EditSeq::new();
        edit_b.retain(2); // Retain "cd"
        edit_b.delete(2); // Delete "ef"

        let composed = edit_a.compose(&edit_b).unwrap();

        // Test the composition directly
        let mut rope = Rope::from("abcdef");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("cd"));

        // Verify it equals sequential application
        let mut rope2 = Rope::from("abcdef");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_insert() {
        // S0 = "hello"
        // A: Insert "X", retain "hello" -> S1 = "Xhello"
        // B: Retain "X", insert "Y", retain "hello" -> S2 = "XYhello"

        let mut edit_a = EditSeq::new();
        edit_a.insert("X");
        edit_a.retain(5);

        let mut edit_b = EditSeq::new();
        edit_b.retain(1);
        edit_b.insert("Y");
        edit_b.retain(5);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("hello");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("XYhello"));
    }

    #[test]
    fn edits_compose_insert_delete() {
        // S0 = "abc"
        // A: Retain "a", insert "XYZ", retain "bc" -> S1 = "aXYZbc"
        // B: Retain "a", delete "XY", retain "Zbc" -> S2 = "aZbc"

        let mut edit_a = EditSeq::new();
        edit_a.retain(1);
        edit_a.insert("XYZ");
        edit_a.retain(2);

        let mut edit_b = EditSeq::new();
        edit_b.retain(1);
        edit_b.delete(2);
        edit_b.retain(3);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("abc");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("aZbc"));

        // Verify property: apply(apply(S, A), B) = apply(S, compose(A, B))
        let mut rope2 = Rope::from("abc");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_complex() {
        // S0 = "Hello, world!"
        // A: Delete "H", insert "Y", retain "ello", insert "w", delete ", world!", insert " and pink"
        //    -> S1 = "Yellow and pink"
        // B: Retain "Yellow", delete " and", retain " pink"
        //    -> S2 = "Yellow pink"

        let mut edit_a = EditSeq::new();
        edit_a.delete(1);
        edit_a.insert("Y");
        edit_a.retain(4);
        edit_a.insert("w");
        edit_a.delete(8);
        edit_a.insert(" and pink");

        let mut edit_b = EditSeq::new();
        edit_b.retain(6);
        edit_b.delete(4);
        edit_b.retain(5);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("Hello, world!");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Yellow pink"));

        // Verify property
        let mut rope2 = Rope::from("Hello, world!");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_length_mismatch() {
        let mut edit_a = EditSeq::new();
        edit_a.retain(5);

        let mut edit_b = EditSeq::new();
        edit_b.retain(3);

        assert!(edit_a.compose(&edit_b).is_err());
    }

    fn gen_edit_seq(
        u: &mut arbitrary::Unstructured<'_>,
        source_bytes: usize,
    ) -> arbitrary::Result<EditSeq> {
        let mut edits = EditSeq::new();
        let mut remaining = source_bytes;

        while remaining > 0 || u.arbitrary::<bool>()? {
            let op_type = u.int_in_range(0..=2)?;

            match op_type {
                // Retain
                0 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    edits.retain(n);
                    remaining -= n;
                }
                // Delete
                1 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    edits.delete(n);
                    remaining -= n;
                }
                // Insert
                2 => {
                    let len = u.int_in_range(1..=10)?;
                    let s: String = (0..len)
                        .map(|_| u.int_in_range(b'a'..=b'z').map(|b| char::from(b)))
                        .collect::<arbitrary::Result<_>>()?;
                    edits.insert(&s);

                    // If we've consumed all source chars, we can stop
                    if remaining == 0 {
                        break;
                    }
                }
                _ => {
                    // If remaining > 0, we must consume it
                    if remaining > 0 {
                        continue;
                    }
                    break;
                }
            }

            // Sometimes stop early to avoid making sequences too long
            if remaining == 0 && u.int_in_range(0..=2)? == 0 {
                break;
            }
        }

        // Ensure we consumed all source chars
        if remaining > 0 {
            edits.retain(remaining);
        }

        Ok(edits)
    }

    #[test]
    fn prop_compose() {
        arbtest::arbtest(|u| {
            // Generate a random initial rope
            let s_len = u.int_in_range(0..=50)?;
            let s: String = (0..s_len)
                .map(|_| u.int_in_range(b'a'..=b'z').map(|b| char::from(b)))
                .collect::<arbitrary::Result<_>>()?;
            let rope = Rope::from(s.as_str());

            // Generate edit sequence A that's valid for the rope
            let edit_a = gen_edit_seq(u, rope.len())?;

            // Apply A to get intermediate rope
            let mut rope1 = rope.clone();
            edit_a.apply(&mut rope1).expect("edit_a.apply failed");

            // Generate edit sequence B that's valid for the intermediate rope
            let edit_b = gen_edit_seq(u, rope1.len())?;

            // Test property: apply(apply(S, A), B) = apply(S, compose(A, B))
            let mut rope_sequential = rope.clone();
            edit_a
                .apply(&mut rope_sequential)
                .expect("sequential: edit_a.apply failed");
            edit_b
                .apply(&mut rope_sequential)
                .expect("sequential: edit_b.apply failed");

            let composed = edit_a.compose(&edit_b).expect("compose failed");
            let mut rope_composed = rope.clone();
            composed
                .apply(&mut rope_composed)
                .expect("composed: apply failed");

            assert_eq!(
                rope_sequential,
                rope_composed,
                "Property violation: apply(apply(S, A), B) != apply(S, compose(A, B))\n\
                 S = {:?}\n\
                 A = source:{} target:{}\n\
                 B = source:{} target:{}",
                rope.to_string(),
                edit_a.source_bytes,
                edit_a.target_bytes,
                edit_b.source_bytes,
                edit_b.target_bytes,
            );

            Ok(())
        });
    }
}
