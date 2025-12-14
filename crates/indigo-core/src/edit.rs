//! Abstract text editing interface.

use crate::ot::OperationSeq;
use ropey::Rope;
use std::{convert::Infallible, ops::Range};

pub trait Edit {
    type Insert;
    type Delete;
    type Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insert, Self::Error>;
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Delete, Self::Error>;
}

pub trait Collab: Edit {
    type Anchor;
    fn integrate_insert(&mut self, insert: &Self::Insert) -> Result<(), Self::Error>;
    fn integrate_delete(&mut self, delete: &Self::Delete) -> Result<(), Self::Error>;
    fn create_anchor(&self, offset: usize) -> Self::Anchor;
    fn resolve_anchor(&self, anchor: &Self::Anchor) -> Option<usize>;
}

pub struct StdText(pub String);

impl Edit for StdText {
    type Insert = ();
    type Delete = ();
    type Error = Infallible;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insert, Self::Error> {
        self.0.insert_str(offset, text);
        Ok(())
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Delete, Self::Error> {
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

pub struct OtInsert {
    pub text: String,
    pub ops: OperationSeq,
    pub version: usize,
}

pub struct OtDelete {
    pub ops: OperationSeq,
    pub version: usize,
}

pub struct OtAnchor {
    pub byte_offset: usize,
    pub version: usize,
}

impl Edit for OtText {
    type Insert = OtInsert;
    type Delete = OtDelete;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insert, Self::Error> {
        let version = self.version();
        let mut ops = OperationSeq::new();
        ops.retain(offset);
        ops.insert(text);
        ops.retain_rest(&self.rope);
        ops.apply(&mut self.rope)?;
        self.ot.push(ops.clone());
        Ok(OtInsert {
            text: String::from(text),
            ops,
            version,
        })
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Delete, Self::Error> {
        let version = self.version();
        let mut ops = OperationSeq::new();
        ops.retain(range.start);
        ops.delete(range.end - range.start);
        ops.retain_rest(&self.rope);
        ops.apply(&mut self.rope)?;
        self.ot.push(ops.clone());
        Ok(OtDelete { ops, version })
    }
}

impl Collab for OtText {
    type Anchor = OtAnchor;
    fn integrate_insert(&mut self, insert: &Self::Insert) -> Result<(), Self::Error> {
        todo!()
    }
    fn integrate_delete(&mut self, delete: &Self::Delete) -> Result<(), Self::Error> {
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

pub struct CrdtInsert {
    pub text: String,
    pub crdt: cola::Insertion,
}

pub struct CrdtDelete {
    pub crdt: cola::Deletion,
}

pub struct CrdtAnchor(pub cola::Anchor);

impl Edit for CrdtText {
    type Insert = CrdtInsert;
    type Delete = CrdtDelete;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insert, Self::Error> {
        self.rope.insert(offset, text);
        let insert = self.crdt.inserted(offset, text.len());
        Ok(CrdtInsert {
            text: String::from(text),
            crdt: insert,
        })
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Delete, Self::Error> {
        self.rope.remove(range.clone());
        let delete = self.crdt.deleted(range);
        Ok(CrdtDelete { crdt: delete })
    }
}

impl Collab for CrdtText {
    type Anchor = CrdtAnchor;
    fn integrate_insert(&mut self, insert: &Self::Insert) -> Result<(), Self::Error> {
        if let Some(byte_offset) = self.crdt.integrate_insertion(&insert.crdt) {
            self.rope.insert(byte_offset, &insert.text);
        }
        Ok(())
    }
    fn integrate_delete(&mut self, delete: &Self::Delete) -> Result<(), Self::Error> {
        let ranges = self.crdt.integrate_deletion(&delete.crdt);
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

        let text1_delete = text1.delete(0..5)?;
        let text1_insert = text1.insert(0, "Goodbye")?;
        assert_eq!(text1.rope, Rope::from("Goodbye"));

        let text2_insert = text2.insert(5, ", world!")?;
        assert_eq!(text2.rope, Rope::from("Hello, world!"));

        text1.integrate_insert(&text2_insert)?;
        // Integrating remote edits out of order still works
        text2.integrate_insert(&text1_insert)?;
        text2.integrate_delete(&text1_delete)?;
        assert_eq!(text1.rope, Rope::from("Goodbye, world!"));
        assert_eq!(text2.rope, Rope::from("Goodbye, world!"));
        Ok(())
    }
}
