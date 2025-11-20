pub mod command;
pub mod goto;
pub mod insert;
pub mod normal;
pub mod seek;

use crate::mode::{
    command::CommandMode, goto::GotoMode, insert::InsertMode, normal::NormalMode, seek::SeekMode,
};
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
