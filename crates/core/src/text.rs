use crate::{
    history::History,
    ot::{self, EditSeq},
};
use ropey::Rope;
use std::ops::Deref;

#[derive(Default)]
pub struct Text {
    rope: Rope,
    history: History<EditSeq>,
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

    pub fn edit(&mut self, edit: EditSeq) -> Result<(), ot::Error> {
        edit.apply(&mut self.rope)?;
        self.history.push(edit);
        Ok(())
    }

    pub fn undo(&mut self) -> Result<bool, ot::Error> {
        if let Some(edit) = self.history.undo() {
            edit.apply(&mut self.rope)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> Result<bool, ot::Error> {
        if let Some(edit) = self.history.redo() {
            edit.apply(&mut self.rope)?;
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
        let history = History::new();
        Self { rope, history }
    }
}

impl From<Rope> for Text {
    fn from(rope: Rope) -> Self {
        let history = History::new();
        Self { rope, history }
    }
}
