use crate::{
    edit::{Collab, Edit, OtAnchor, OtDeletion, OtInsertion},
    ot::OperationSeq,
    history::History,
};
use ropey::Rope;
use std::ops::{Deref, Range};

#[derive(Debug, Default)]
struct BidiOperationSeq {
    /// Inverted operations. Apply to undo the edit.
    undo: OperationSeq,
    /// Original operations. Apply to perform the edit.
    redo: OperationSeq,
}

impl Extend<Self> for BidiOperationSeq {
    fn extend<T>(&mut self, opss: T)
    where
        T: IntoIterator<Item = Self>,
    {
        for ops in opss {
            self.undo = ops.undo.compose(&self.undo).unwrap();
            self.redo = self.redo.compose(&ops.redo).unwrap();
        }
    }
}

#[derive(Debug, Default)]
pub struct Text {
    rope: Rope,
    history: History<BidiOperationSeq>,
    log: Vec<OperationSeq>,
}

impl Text {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    pub fn apply(&mut self, ops: &OperationSeq) -> anyhow::Result<()> {
        let undo = ops.invert(&self.rope)?;
        ops.apply(&mut self.rope)?;
        self.history.push(BidiOperationSeq {
            redo: ops.clone(),
            undo,
        });
        self.log.push(ops.clone());
        Ok(())
    }

    pub fn commit(&mut self) {
        self.history.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        if let Some(opss) = self.history.undo() {
            for ops in opss.iter().rev() {
                ops.undo.apply(&mut self.rope)?;
                self.log.push(ops.undo.clone());
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        if let Some(opss) = self.history.redo() {
            for ops in opss {
                ops.redo.apply(&mut self.rope)?;
                self.log.push(ops.undo.clone());
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[must_use]
    pub fn version(&self) -> usize {
        self.log.len()
    }

    #[must_use]
    pub fn ops_since(&self, version: usize) -> Option<&[OperationSeq]> {
        self.log.get(version..)
    }
}

impl Edit for Text {
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
        self.log.push(ops.clone());
        let redo = ops.clone();
        let undo = redo.invert(&self.rope)?;
        self.history.push(BidiOperationSeq { undo, redo });
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
        self.log.push(ops.clone());
        let redo = ops.clone();
        let undo = redo.invert(&self.rope)?;
        self.history.push(BidiOperationSeq { undo, redo });
        Ok(OtDeletion { ops, version })
    }
}

impl Collab for Text {
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
        for ops in self.ops_since(anchor.version)? {
            byte_offset = ops.transform_byte_offset(byte_offset);
        }
        Some(byte_offset)
    }
}

impl Deref for Text {
    type Target = Rope;

    fn deref(&self) -> &Self::Target {
        &self.rope
    }
}

impl<'a> From<&'a str> for Text {
    fn from(str: &'a str) -> Self {
        let rope = Rope::from(str);
        Self {
            rope,
            ..Self::default()
        }
    }
}

impl From<Rope> for Text {
    fn from(rope: Rope) -> Self {
        Self {
            rope,
            ..Self::default()
        }
    }
}
