use crate::{history::History, ot::EditSeq};
use ropey::Rope;
use std::ops::Deref;

#[derive(Debug, Default)]
struct Edit {
    undo: EditSeq,
    redo: EditSeq,
}

// TODO: Implement `EditSeq::compose`, replace `History<Edit>` with `History<Edit, Edit>`. Or maybe
// not, since that means using `unwrap` in this impl?
// impl Extend<Self> for Edit {
//     fn extend<T>(&mut self, edits: T)
//     where
//         T: IntoIterator<Item = Self>,
//     {
//         for edit in edits {
//             self.undo = self.undo.compose(&edit.undo).unwrap();
//             self.redo = self.redo.compose(&edit.redo).unwrap();
//         }
//     }
// }

#[derive(Debug)]
pub struct Text {
    rope: Rope,
    original: Rope,
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
        self.rope != self.original
    }

    pub fn set_unmodified(&mut self) {
        self.original = self.rope.clone();
    }

    pub fn edit(&mut self, edit: &EditSeq) -> anyhow::Result<()> {
        let undo = edit.invert(&self.rope)?;
        edit.apply(&mut self.rope)?;
        self.history.push(Edit {
            redo: edit.clone(),
            undo,
        });
        Ok(())
    }

    pub fn commit(&mut self) {
        self.history.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        if let Some(edits) = self.history.undo() {
            for edit in edits.iter().rev() {
                edit.undo.apply(&mut self.rope)?;
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
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Default for Text {
    fn default() -> Self {
        let rope = Rope::default();
        Self {
            original: rope.clone(),
            rope,
            history: History::default(),
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
            original: rope.clone(),
            rope,
            ..Self::default()
        }
    }
}

impl From<Rope> for Text {
    fn from(rope: Rope) -> Self {
        Self {
            original: rope.clone(),
            rope,
            ..Self::default()
        }
    }
}
