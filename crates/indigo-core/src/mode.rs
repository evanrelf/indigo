pub mod command;
pub mod goto;
pub mod insert;
pub mod normal;
pub mod seek;

use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    mode::{goto::GotoMode, seek::SeekMode},
    text::Text,
};
use ropey::Rope;
use std::num::NonZeroUsize;

// TODO: Mode stack so that seek mode can look at normal mode's count instead of keeping a copy?

pub enum Mode {
    Normal(NormalMode),
    Seek(SeekMode),
    Goto(GotoMode),
    Insert(InsertMode),
    Command(CommandMode),
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> Option<NonZeroUsize> {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Seek(seek_mode) => seek_mode.count,
            _ => None,
        }
    }

    pub fn set_count(&mut self, count: Option<NonZeroUsize>) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Seek(seek_mode) => seek_mode.count = count,
            _ => {}
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

#[derive(Default)]
pub struct NormalMode {
    pub count: Option<NonZeroUsize>,
}

#[derive(Default)]
pub struct InsertMode {}

#[derive(Default)]
pub struct CommandMode {
    text: Text,
    cursor: CursorState,
}

impl CommandMode {
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.text.rope()
    }

    pub fn cursor(&self) -> Cursor<'_> {
        let cursor = Cursor::new(&self.text, &self.cursor)
            .expect("Command mode text and cursor state are always kept valid");
        cursor.assert_invariants().unwrap();
        cursor
    }

    pub fn cursor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.cursor)
            .expect("Command mode text and cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }
}
