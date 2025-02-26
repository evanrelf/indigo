use crate::cursor::{Cursor, CursorMut, RawCursor};
use ropey::Rope;
use std::num::NonZeroUsize;

pub enum Mode {
    Normal(NormalMode),
    Insert,
    Command(CommandMode),
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> NonZeroUsize {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Insert | Self::Command(_) => NonZeroUsize::MIN,
        }
    }

    pub fn set_count(&mut self, count: NonZeroUsize) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Insert | Self::Command(_) => {}
        }
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
pub struct CommandMode {
    rope: Rope,
    cursor: RawCursor,
}

impl CommandMode {
    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn cursor(&self) -> &RawCursor {
        &self.cursor
    }

    pub fn with_cursor<T>(&self, func: impl Fn(&Cursor) -> T) -> T {
        let cursor = Cursor::new(&self.rope, self.cursor.gap_index).unwrap();
        func(&cursor)
    }

    pub fn with_cursor_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let mut cursor = CursorMut::new(&mut self.rope, self.cursor.gap_index).unwrap();
        let result = func(&mut cursor);
        cursor.assert_valid();
        self.cursor = cursor.raw();
        result
    }
}
