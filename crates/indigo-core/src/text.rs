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
