use crate::cursor_view::{Cursor, CursorMut, CursorState};
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
    cursor: CursorState,
}

impl CommandMode {
    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn cursor(&self) -> Cursor {
        Cursor::new(&self.text, &self.cursor).unwrap()
    }

    // TODO: Use `cursor_mut` instead of `with_cursor_mut`, run `assert_invariants` _before_ handing
    // out view? In theory less correct, in practice any invariant violations will be caught on the
    // next pass.
    // #[must_use]
    // pub fn cursor_mut(&mut self) -> CursorMut {
    //     CursorMut::new(&mut self.text, &mut self.cursor).unwrap()
    // }

    pub fn with_cursor_mut<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let mut cursor = CursorMut::new(&mut self.text, &mut self.cursor).unwrap();
        let result = func(&mut cursor);
        cursor.assert_invariants();
        result
    }
}
