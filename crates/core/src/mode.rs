use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    text::Text,
};
use std::num::NonZeroUsize;

pub enum ModeKind {
    Normal,
    Insert,
    Command,
}

pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
    Command(CommandMode),
}

impl Mode {
    #[must_use]
    pub fn kind(&self) -> ModeKind {
        match self {
            Self::Normal(_) => ModeKind::Normal,
            Self::Insert(_) => ModeKind::Insert,
            Self::Command(_) => ModeKind::Command,
        }
    }

    #[must_use]
    pub fn count(&self) -> NonZeroUsize {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Insert(_) | Self::Command(_) => NonZeroUsize::MIN,
        }
    }

    pub fn set_count(&mut self, count: NonZeroUsize) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Insert(_) | Self::Command(_) => {}
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

pub struct NormalMode {
    pub count: NonZeroUsize,
}

impl Default for NormalMode {
    fn default() -> Self {
        Self {
            count: NonZeroUsize::MIN,
        }
    }
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
    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn cursor(&self) -> Cursor {
        let cursor = Cursor::new(&self.text, &self.cursor).unwrap();
        cursor.assert_invariants().unwrap();
        cursor
    }

    pub fn cursor_mut(&mut self) -> CursorMut {
        let cursor = CursorMut::new(&mut self.text, &mut self.cursor).unwrap();
        cursor.assert_invariants().unwrap();
        cursor
    }
}
