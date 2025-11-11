use crate::{history::History, ot::EditSeq};
use ropey::Rope;
use std::ops::Deref;

#[derive(Debug)]
struct Edit {
    undo: EditSeq,
    redo: EditSeq,
}

#[derive(Debug, Default)]
pub struct Text {
    rope: Rope,
    original: Option<Rope>,
    history: History<Edit>,
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

    #[must_use]
    pub fn is_modified(&self) -> bool {
        self.original
            .as_ref()
            .is_some_and(|original| self.rope != *original)
    }

    pub fn set_unmodified(&mut self) {
        self.original = None;
    }

    pub fn edit(&mut self, edit: &EditSeq) -> anyhow::Result<()> {
        if self.original.is_none() {
            self.original = Some(self.rope.clone());
        }
        let undo = edit.invert(&self.rope)?;
        edit.apply(&mut self.rope)?;
        self.history.push(Edit {
            redo: edit.clone(),
            undo,
        });
        Ok(())
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        if let Some(entry) = self.history.undo() {
            entry.undo.apply(&mut self.rope)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        if let Some(entry) = self.history.redo() {
            entry.redo.apply(&mut self.rope)?;
            Ok(true)
        } else {
            Ok(false)
        }
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
