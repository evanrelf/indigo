use crate::{
    edit::{Edit, EditSeq, OtDeletion, OtInsertion, OtText},
    history::History,
};
use indigo_wrap::WMut;
use ropey::Rope;
use std::ops::{Deref, Range};

#[derive(Debug, Default)]
struct BidiEditSeq {
    /// Inverted edit. Apply to undo the edit.
    undo: EditSeq,
    /// Original edit. Apply to perform the edit.
    redo: EditSeq,
}

impl Extend<Self> for BidiEditSeq {
    fn extend<T>(&mut self, edits: T)
    where
        T: IntoIterator<Item = Self>,
    {
        for edit in edits {
            self.undo = edit.undo.compose(&self.undo).unwrap();
            self.redo = self.redo.compose(&edit.redo).unwrap();
        }
    }
}

#[derive(Debug, Default)]
pub struct Text {
    rope: Rope,
    history: History<BidiEditSeq>,
    edit_log: Vec<EditSeq>,
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

    pub fn edit(&mut self, edit: &EditSeq) -> anyhow::Result<()> {
        let undo = edit.invert(&self.rope)?;
        edit.apply(&mut self.rope)?;
        self.history.push(BidiEditSeq {
            redo: edit.clone(),
            undo,
        });
        self.edit_log.push(edit.clone());
        Ok(())
    }

    pub fn commit(&mut self) {
        self.history.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        if let Some(edits) = self.history.undo() {
            for edit in edits.iter().rev() {
                edit.undo.apply(&mut self.rope)?;
                self.edit_log.push(edit.undo.clone());
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        if let Some(edits) = self.history.redo() {
            for edit in edits {
                edit.redo.apply(&mut self.rope)?;
                self.edit_log.push(edit.undo.clone());
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[must_use]
    pub fn version(&self) -> usize {
        self.edit_log.len()
    }

    #[must_use]
    pub fn edits_since(&self, version: usize) -> Option<&[EditSeq]> {
        self.edit_log.get(version..)
    }
}

impl Edit for Text {
    type Insertion = OtInsertion;
    type Deletion = OtDeletion;
    type Error = anyhow::Error;
    fn insert(&mut self, offset: usize, text: &str) -> Result<Self::Insertion, Self::Error> {
        let mut ot_text: OtText<'_, WMut> = OtText {
            rope: &mut self.rope,
            ot: &mut self.edit_log,
        };
        let insertion = ot_text.insert(offset, text)?;
        let redo = self
            .edit_log
            .last()
            .expect("`OtText::insert` just pushed one `EditSeq`")
            .clone();
        let undo = redo.invert(&self.rope)?;
        self.history.push(BidiEditSeq { undo, redo });
        Ok(insertion)
    }
    fn delete(&mut self, range: Range<usize>) -> Result<Self::Deletion, Self::Error> {
        let mut ot_text: OtText<'_, WMut> = OtText {
            rope: &mut self.rope,
            ot: &mut self.edit_log,
        };
        let deletion = ot_text.delete(range)?;
        let redo = self
            .edit_log
            .last()
            .expect("`OtText::delete` just pushed one `EditSeq`")
            .clone();
        let undo = redo.invert(&self.rope)?;
        self.history.push(BidiEditSeq { undo, redo });
        Ok(deletion)
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
