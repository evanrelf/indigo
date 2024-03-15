use ropey::Rope;
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub struct Text {
    rope: Rope,
}

impl Deref for Text {
    type Target = Rope;

    fn deref(&self) -> &Self::Target {
        &self.rope
    }
}

impl DerefMut for Text {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.rope
    }
}

impl Default for Text {
    fn default() -> Self {
        Self {
            rope: Rope::from("\n"),
        }
    }
}
