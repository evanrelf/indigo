use crate::ot::{self, EditSeq};
use ropey::Rope;
use std::ops::Deref;

#[derive(Debug, Default, PartialEq)]
pub struct Text {
    rope: Rope,
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

    pub fn edit(&mut self, edit: &EditSeq) -> Result<(), ot::Error> {
        edit.apply(&mut self.rope)?;
        Ok(())
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
        Self { rope }
    }
}

impl From<Rope> for Text {
    fn from(rope: Rope) -> Self {
        Self { rope }
    }
}
