//! Operational transformations.
//!
//! <https://en.wikipedia.org/wiki/Operational_transformation>

use ropey::Rope;
use std::{
    cmp::min,
    convert::Infallible,
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

pub trait Collab: Edit {
    type Anchor;
    fn integrate_insertion(&mut self, insertion: &Self::Insertion) -> Result<(), Self::Error>;
    fn integrate_deletion(&mut self, deletion: &Self::Deletion) -> Result<(), Self::Error>;
    fn create_anchor(&self, offset: usize) -> Self::Anchor;
    fn resolve_anchor(&self, anchor: &Self::Anchor) -> Option<usize>;
}

pub struct StdText(pub String);

impl Edit for StdText {
    type Insertion = ();
    type Deletion = ();
    type Error = Infallible;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error> {
        self.0.insert_str(offset, text);
        Ok(())
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error> {
        self.0.replace_range(range, "");
        Ok(())
    }
}

pub struct OtText {
    pub rope: Rope,
    pub ot: Vec<OperationSeq>,
}

impl OtText {
    #[must_use]
    pub fn version(&self) -> usize {
        self.ot.len()
    }
}

pub struct OtInsertion {
    pub text: String,
    pub ops: OperationSeq,
    pub version: usize,
}

pub struct OtDeletion {
    pub ops: OperationSeq,
    pub version: usize,
}

pub struct OtAnchor {
    pub byte_offset: usize,
    pub version: usize,
}

impl Edit for OtText {
    type Insertion = OtInsertion;
    type Deletion = OtDeletion;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error> {
        let version = self.version();
        let mut ops = OperationSeq::new();
        ops.retain(offset);
        ops.insert(text);
        ops.retain_rest(&self.rope);
        ops.apply(&mut self.rope)?;
        self.ot.push(ops.clone());
        Ok(OtInsertion {
            text: String::from(text),
            ops,
            version,
        })
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error> {
        let version = self.version();
        let mut ops = OperationSeq::new();
        ops.retain(range.start);
        ops.delete(range.end - range.start);
        ops.retain_rest(&self.rope);
        ops.apply(&mut self.rope)?;
        self.ot.push(ops.clone());
        Ok(OtDeletion { ops, version })
    }
}

impl Collab for OtText {
    type Anchor = OtAnchor;
    fn integrate_insertion(&mut self, insertion: &Self::Insertion) -> Result<(), Self::Error> {
        todo!()
    }
    fn integrate_deletion(&mut self, deletion: &Self::Deletion) -> Result<(), Self::Error> {
        todo!()
    }
    fn create_anchor(&self, offset: usize) -> Self::Anchor {
        OtAnchor {
            byte_offset: offset,
            version: self.version(),
        }
    }
    fn resolve_anchor(&self, anchor: &Self::Anchor) -> Option<usize> {
        let mut byte_offset = anchor.byte_offset;
        for ops in self.ot.get(anchor.version..)? {
            byte_offset = ops.transform_byte_offset(byte_offset);
        }
        Some(byte_offset)
    }
}

pub struct CrdtText {
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

pub struct CrdtAnchor(pub cola::Anchor);

impl Edit for CrdtText {
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

impl Collab for CrdtText {
    type Anchor = CrdtAnchor;
    fn integrate_insertion(&mut self, insertion: &Self::Insertion) -> Result<(), Self::Error> {
        if let Some(byte_offset) = self.crdt.integrate_insertion(&insertion.crdt) {
            self.rope.insert(byte_offset, &insertion.text);
        }
        Ok(())
    }
    fn integrate_deletion(&mut self, deletion: &Self::Deletion) -> Result<(), Self::Error> {
        let ranges = self.crdt.integrate_deletion(&deletion.crdt);
        for range in ranges.into_iter().rev() {
            self.rope.remove(range);
        }
        Ok(())
    }
    fn create_anchor(&self, byte_offset: usize) -> Self::Anchor {
        CrdtAnchor(self.crdt.create_anchor(byte_offset, cola::AnchorBias::Left))
    }
    fn resolve_anchor(&self, anchor: &Self::Anchor) -> Option<usize> {
        self.crdt.resolve_anchor(anchor.0)
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

// TODO: Remove
pub type EditSeq = OperationSeq;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct OperationSeq {
    ops: Vec<Operation>,
    source_bytes: usize,
    target_bytes: usize,
}

impl OperationSeq {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            ops: Vec::with_capacity(capacity),
            source_bytes: 0,
            target_bytes: 0,
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.source_bytes += byte_length;
        self.target_bytes += byte_length;

        if let Some(Operation::Retain(n)) = self.ops.last_mut() {
            *n += byte_length;
        } else {
            self.ops.push(Operation::Retain(byte_length));
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

        if let Some(Operation::Delete(n)) = self.ops.last_mut() {
            *n += byte_length;
        } else {
            self.ops.push(Operation::Delete(byte_length));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.target_bytes += text.len();

        if let Some(Operation::Insert(last)) = self.ops.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            *last = Rc::from(s);
        } else {
            self.ops.push(Operation::Insert(Rc::from(text)));
        }
    }

    pub fn push(&mut self, op: Operation) {
        match op {
            Operation::Retain(n) => self.retain(n),
            Operation::Delete(n) => self.delete(n),
            Operation::Insert(s) => self.insert(&s),
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

        let mut iter1 = self.ops.iter().cloned();
        let mut iter2 = other.ops.iter().cloned();

        let mut op1 = iter1.next();
        let mut op2 = iter2.next();

        while op1.is_some() || op2.is_some() {
            match (&mut op1, &mut op2) {
                // Delete from first: pass through, doesn't consume from second
                (Some(Operation::Delete(n)), _) => {
                    result.delete(*n);
                    op1 = iter1.next();
                }

                // Insert from second: pass through, doesn't consume from first
                (_, Some(Operation::Insert(s))) => {
                    result.insert(s);
                    op2 = iter2.next();
                }

                // Retain + Retain: both consume, output retain
                (Some(Operation::Retain(n1)), Some(Operation::Retain(n2))) => {
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
                (Some(Operation::Retain(n1)), Some(Operation::Delete(n2))) => {
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
                (Some(Operation::Insert(s1)), Some(Operation::Retain(n2))) => {
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
                (Some(Operation::Insert(s1)), Some(Operation::Delete(n2))) => {
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

                (None, Some(Operation::Retain(_) | Operation::Delete(_))) => {
                    anyhow::bail!(Error::ComposeSecondExpectsMore);
                }

                (Some(Operation::Retain(_) | Operation::Insert(_)), None) => {
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

        for op in &self.ops {
            match op {
                Operation::Retain(n) => {
                    position += n;
                }
                Operation::Delete(n) => {
                    if position < byte_offset {
                        byte_offset -= n;
                    }
                }
                Operation::Insert(s) => {
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

        for op in &self.ops {
            match op {
                Operation::Retain(n) => inverted.retain(*n),
                Operation::Delete(n) => {
                    let start = inverted.target_bytes;
                    let end = start + n;
                    let s = rope.slice(start..end).to_string();
                    inverted.insert(&s);
                }
                Operation::Insert(s) => inverted.delete(s.len()),
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

        for op in &self.ops {
            byte_offset = op.apply(byte_offset, rope)?;
        }

        Ok(())
    }
}

impl Deref for OperationSeq {
    type Target = [Operation];

    fn deref(&self) -> &Self::Target {
        &self.ops
    }
}

impl IntoIterator for OperationSeq {
    type Item = Operation;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.ops.into_iter()
    }
}

impl<'a> IntoIterator for &'a OperationSeq {
    type Item = &'a Operation;

    type IntoIter = std::slice::Iter<'a, Operation>;

    fn into_iter(self) -> Self::IntoIter {
        self.ops.iter()
    }
}

impl Extend<Operation> for OperationSeq {
    fn extend<T>(&mut self, ops: T)
    where
        T: IntoIterator<Item = Operation>,
    {
        for op in ops {
            self.push(op);
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    Retain(usize),
    Delete(usize),
    Insert(Rc<str>),
}

impl Operation {
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
    fn edit_std() -> anyhow::Result<()> {
        let mut text = StdText(String::from("The quick brown fox"));
        text.delete(4..9)?;
        assert_eq!(&text.0, "The  brown fox");
        text.insert(4, "cute")?;
        assert_eq!(&text.0, "The cute brown fox");
        text.delete(9..14)?;
        assert_eq!(&text.0, "The cute  fox");
        text.insert(9, "white")?;
        assert_eq!(&text.0, "The cute white fox");
        Ok(())
    }

    #[test]
    fn edit_ot() -> anyhow::Result<()> {
        let mut text = OtText {
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
        let mut text = CrdtText {
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
    fn collab_crdt() -> anyhow::Result<()> {
        let rope = Rope::from("Hello");
        let mut text1 = CrdtText {
            crdt: cola::Replica::new(1, rope.len()),
            rope: rope.clone(),
        };
        let mut text2 = CrdtText {
            crdt: text1.crdt.fork(2),
            rope: rope.clone(),
        };

        let text1_deletion = text1.delete(0..5)?;
        let text1_insertion = text1.insert(0, "Goodbye")?;
        assert_eq!(text1.rope, Rope::from("Goodbye"));

        let text2_insertion = text2.insert(5, ", world!")?;
        assert_eq!(text2.rope, Rope::from("Hello, world!"));

        text1.integrate_insertion(&text2_insertion)?;
        // Integrating remote edits out of order still works
        text2.integrate_insertion(&text1_insertion)?;
        text2.integrate_deletion(&text1_deletion)?;
        assert_eq!(text1.rope, Rope::from("Goodbye, world!"));
        assert_eq!(text2.rope, Rope::from("Goodbye, world!"));
        Ok(())
    }

    #[test]
    fn operation_seq_push() {
        let mut ops = OperationSeq::new();
        ops.extend([
            Operation::Insert(Rc::from("Hello,")),
            Operation::Insert(Rc::from(" world!")),
            Operation::Retain(42),
            Operation::Retain(58),
            Operation::Delete(4),
            Operation::Delete(4),
            Operation::Delete(4),
        ]);
        assert_eq!(
            *ops,
            [
                Operation::Insert(Rc::from("Hello, world!")),
                Operation::Retain(100),
                Operation::Delete(12),
            ]
        );
    }

    #[test]
    fn ops_transform_offset() {
        let mut rope = Rope::from("Hello, world!");

        let mut byte_offset = 9;
        assert_eq!(rope.char(byte_offset), 'r');

        let mut ops = OperationSeq::new();
        ops.delete("Hello, ".len());
        ops.retain("world".len());
        ops.insert("!!!");
        ops.retain("!".len());

        ops.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("world!!!!"));

        assert_eq!(rope.get_char(byte_offset).ok(), None);
        byte_offset = ops.transform_byte_offset(byte_offset);
        assert_eq!(byte_offset, 2);
        assert_eq!(rope.char(byte_offset), 'r');

        let mut ops = OperationSeq::new();
        ops.delete("w".len());
        ops.insert("the whole w");
        ops.retain_rest(&rope);
        ops.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("the whole world!!!!"));

        assert_eq!(rope.char(byte_offset), 'e');
        byte_offset = ops.transform_byte_offset(byte_offset);
        assert_eq!(byte_offset, 12);
        assert_eq!(rope.char(byte_offset), 'r');
    }

    #[test]
    fn ops_invert() {
        let rope1 = Rope::from("Hello, world!");

        let mut ops = OperationSeq::new();
        ops.delete("H".len());
        ops.insert("Y");
        ops.retain("ello".len());
        ops.insert("w");
        ops.delete(", world!".len());
        ops.insert(" and pink");

        let mut rope2 = rope1.clone();
        ops.apply(&mut rope2).unwrap();
        assert_eq!(rope2, Rope::from("Yellow and pink"));

        assert_eq!(ops.invert(&rope1).unwrap().invert(&rope2).unwrap(), ops);

        let mut rope3 = rope2.clone();
        ops.invert(&rope1).unwrap().apply(&mut rope3).unwrap();
        assert_eq!(rope3, Rope::from("Hello, world!"));
    }

    #[test]
    fn ops_apply() {
        let mut ops = OperationSeq::new();
        ops.retain(7);
        ops.delete("world".len());
        ops.insert("Evan");
        ops.delete("!".len());
        ops.insert("...");
        ops.insert("?");

        let mut rope = Rope::from("Hello, world!");
        ops.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));

        let mut rope = Rope::from("Hello, world!");
        ops.retain(1);
        assert!(ops.apply(&mut rope).is_err());
    }

    #[test]
    fn operation_apply() {
        let mut rope = Rope::from("Hello, world!");
        let byte_offset = 0;
        let byte_offset = Operation::Retain(7).apply(byte_offset, &mut rope).unwrap();
        let byte_offset = Operation::Delete("world".len())
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = Operation::Insert(Rc::from("Evan"))
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = Operation::Delete("!".len())
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = Operation::Insert(Rc::from("..."))
            .apply(byte_offset, &mut rope)
            .unwrap();
        let byte_offset = Operation::Insert(Rc::from("?"))
            .apply(byte_offset, &mut rope)
            .unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Operation::Retain(1).apply(byte_offset, &mut rope).is_err());
    }

    #[test]
    fn ops_compose_basic() {
        // S0 = "abcdef"
        // A: Delete "ab", retain "cdef" -> S1 = "cdef"
        // B: Retain "cd", delete "ef" -> S2 = "cd"
        // A ∘ B should transform "abcdef" -> "cd"

        let mut ops_a = OperationSeq::new();
        ops_a.delete(2); // Delete "ab"
        ops_a.retain(4); // Retain "cdef"

        let mut ops_b = OperationSeq::new();
        ops_b.retain(2); // Retain "cd"
        ops_b.delete(2); // Delete "ef"

        let composed = ops_a.compose(&ops_b).unwrap();

        // Test the composition directly
        let mut rope = Rope::from("abcdef");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("cd"));

        // Verify it equals sequential application
        let mut rope2 = Rope::from("abcdef");
        ops_a.apply(&mut rope2).unwrap();
        ops_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn ops_compose_insert() {
        // S0 = "hello"
        // A: Insert "X", retain "hello" -> S1 = "Xhello"
        // B: Retain "X", insert "Y", retain "hello" -> S2 = "XYhello"

        let mut ops_a = OperationSeq::new();
        ops_a.insert("X");
        ops_a.retain(5);

        let mut ops_b = OperationSeq::new();
        ops_b.retain(1);
        ops_b.insert("Y");
        ops_b.retain(5);

        let composed = ops_a.compose(&ops_b).unwrap();

        let mut rope = Rope::from("hello");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("XYhello"));
    }

    #[test]
    fn ops_compose_insert_delete() {
        // S0 = "abc"
        // A: Retain "a", insert "XYZ", retain "bc" -> S1 = "aXYZbc"
        // B: Retain "a", delete "XY", retain "Zbc" -> S2 = "aZbc"

        let mut ops_a = OperationSeq::new();
        ops_a.retain(1);
        ops_a.insert("XYZ");
        ops_a.retain(2);

        let mut ops_b = OperationSeq::new();
        ops_b.retain(1);
        ops_b.delete(2);
        ops_b.retain(3);

        let composed = ops_a.compose(&ops_b).unwrap();

        let mut rope = Rope::from("abc");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("aZbc"));

        // Verify property: apply(apply(S, A), B) = apply(S, compose(A, B))
        let mut rope2 = Rope::from("abc");
        ops_a.apply(&mut rope2).unwrap();
        ops_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn ops_compose_complex() {
        // S0 = "Hello, world!"
        // A: Delete "H", insert "Y", retain "ello", insert "w", delete ", world!", insert " and pink"
        //    -> S1 = "Yellow and pink"
        // B: Retain "Yellow", delete " and", retain " pink"
        //    -> S2 = "Yellow pink"

        let mut ops_a = OperationSeq::new();
        ops_a.delete(1);
        ops_a.insert("Y");
        ops_a.retain(4);
        ops_a.insert("w");
        ops_a.delete(8);
        ops_a.insert(" and pink");

        let mut ops_b = OperationSeq::new();
        ops_b.retain(6);
        ops_b.delete(4);
        ops_b.retain(5);

        let composed = ops_a.compose(&ops_b).unwrap();

        let mut rope = Rope::from("Hello, world!");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Yellow pink"));

        // Verify property
        let mut rope2 = Rope::from("Hello, world!");
        ops_a.apply(&mut rope2).unwrap();
        ops_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn ops_compose_length_mismatch() {
        let mut ops_a = OperationSeq::new();
        ops_a.retain(5);

        let mut ops_b = OperationSeq::new();
        ops_b.retain(3);

        assert!(ops_a.compose(&ops_b).is_err());
    }

    fn gen_operation_seq(
        u: &mut arbitrary::Unstructured<'_>,
        source_bytes: usize,
    ) -> arbitrary::Result<OperationSeq> {
        let mut ops = OperationSeq::new();
        let mut remaining = source_bytes;

        while remaining > 0 || u.arbitrary::<bool>()? {
            let op_type = u.int_in_range(0..=2)?;

            match op_type {
                // Retain
                0 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    ops.retain(n);
                    remaining -= n;
                }
                // Delete
                1 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    ops.delete(n);
                    remaining -= n;
                }
                // Insert
                2 => {
                    let len = u.int_in_range(1..=10)?;
                    let s: String = (0..len)
                        .map(|_| u.int_in_range(b'a'..=b'z').map(|b| char::from(b)))
                        .collect::<arbitrary::Result<_>>()?;
                    ops.insert(&s);

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
            ops.retain(remaining);
        }

        Ok(ops)
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

            // Generate operation sequence A that's valid for the rope
            let ops_a = gen_operation_seq(u, rope.len())?;

            // Apply A to get intermediate rope
            let mut rope1 = rope.clone();
            ops_a.apply(&mut rope1).expect("ops_a.apply failed");

            // Generate operation sequence B that's valid for the intermediate rope
            let ops_b = gen_operation_seq(u, rope1.len())?;

            // Test property: apply(apply(S, A), B) = apply(S, compose(A, B))
            let mut rope_sequential = rope.clone();
            ops_a
                .apply(&mut rope_sequential)
                .expect("sequential: ops_a.apply failed");
            ops_b
                .apply(&mut rope_sequential)
                .expect("sequential: ops_b.apply failed");

            let composed = ops_a.compose(&ops_b).expect("compose failed");
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
                ops_a.source_bytes,
                ops_a.target_bytes,
                ops_b.source_bytes,
                ops_b.target_bytes,
            );

            Ok(())
        });
    }
}
