use crate::{history::History, ot::OperationSeq};
use ropey::Rope;
use std::ops::{Deref, Range};

#[derive(Clone, Debug)]
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
            if self.redo.is_empty() {
                *self = ops;
            } else {
                self.undo = ops.undo.compose(&self.undo).unwrap();
                self.redo = self.redo.compose(&ops.redo).unwrap();
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Text {
    rope: Rope,
    history: History<BidiOperationSeq, BidiOperationSeq>,
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

    pub fn insert(&mut self, byte_offset: usize, text: &str) -> anyhow::Result<()> {
        let mut ops = OperationSeq::new();
        ops.retain(byte_offset);
        ops.insert(text);
        ops.retain_rest(&self.rope);
        self.apply(&ops)?;
        Ok(())
    }

    pub fn delete(&mut self, range: Range<usize>) -> anyhow::Result<()> {
        let mut ops = OperationSeq::new();
        ops.retain(range.start);
        ops.delete(range.end - range.start);
        ops.retain_rest(&self.rope);
        self.apply(&ops)?;
        Ok(())
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
        if let Some(ops) = self.history.undo() {
            ops.undo.apply(&mut self.rope)?;
            self.log.push(ops.undo.clone());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        if let Some(ops) = self.history.redo() {
            ops.redo.apply(&mut self.rope)?;
            self.log.push(ops.redo.clone());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[must_use]
    pub fn create_anchor(&self, byte_offset: usize) -> Anchor {
        Anchor::new(self, byte_offset)
    }

    #[must_use]
    pub fn resolve_anchor(&self, anchor: &Anchor) -> Option<usize> {
        anchor.resolve(self)
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

pub struct Anchor {
    byte_offset: usize,
    version: usize,
}

impl Anchor {
    #[must_use]
    pub fn new(text: &Text, byte_offset: usize) -> Self {
        Self {
            byte_offset,
            version: text.version(),
        }
    }

    #[must_use]
    pub fn resolve(&self, text: &Text) -> Option<usize> {
        let mut byte_offset = self.byte_offset;
        for ops in text.ops_since(self.version)? {
            byte_offset = ops.transform_byte_offset(byte_offset);
        }
        Some(byte_offset)
    }
}
