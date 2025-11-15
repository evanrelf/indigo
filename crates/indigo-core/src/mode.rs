use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    text::Text,
};
use std::num::NonZeroUsize;

pub enum Mode {
    Normal(NormalMode),
    Seek(SeekMode),
    Insert(InsertMode),
    Command(CommandMode),
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> Option<NonZeroUsize> {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Seek(_) | Self::Insert(_) | Self::Command(_) => None,
        }
    }

    pub fn set_count(&mut self, count: Option<NonZeroUsize>) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Seek(_) | Self::Insert(_) | Self::Command(_) => {}
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

pub enum SeekSelect {
    Move,
    Extend,
}

pub enum SeekInclude {
    Until,
    Onto,
}

pub enum SeekDirection {
    Prev,
    Next,
}

pub struct SeekMode {
    /// Move and create a new selection, or extend the current one?
    pub select: SeekSelect,
    /// Include seek target in selection, or stop just short?
    pub include: SeekInclude,
    /// Seek forward or backward?
    pub direction: SeekDirection,
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
