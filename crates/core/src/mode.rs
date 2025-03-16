use crate::cursor::Cursor;
use ropey::Rope;
use std::num::NonZeroUsize;

pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
    Command(CommandMode),
}

impl Mode {
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
    text: Rope,
    cursor: Cursor<()>,
}

impl CommandMode {
    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn cursor(&self) -> &Cursor<()> {
        &self.cursor
    }

    pub fn with_cursor<T>(&self, func: impl Fn(&Cursor<&Rope>) -> T) -> T {
        let cursor = Cursor::new(self.cursor.char_offset())
            .try_with(&self.text)
            .unwrap();
        func(&cursor)
    }

    pub fn with_cursor_mut<T>(&mut self, func: impl Fn(&mut Cursor<&mut Rope>) -> T) -> T {
        let mut cursor = Cursor::new(self.cursor.char_offset())
            .try_with(&mut self.text)
            .unwrap();
        let result = func(&mut cursor);
        cursor.assert_valid();
        self.cursor = cursor.without();
        result
    }
}
