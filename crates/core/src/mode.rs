use crate::cursor::RawCursor;
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
