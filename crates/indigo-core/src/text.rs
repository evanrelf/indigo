use crate::ot::EditSeq;
use ropey::Rope;
use std::ops::Deref;

#[derive(Debug, Default)]
pub struct Text {
    rope: Rope,
    original: Option<Rope>,
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
