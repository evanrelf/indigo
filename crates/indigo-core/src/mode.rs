pub mod command;
pub mod goto;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod seek;

use std::num::NonZeroUsize;

// TODO: Mode stack so that seek mode can look at normal mode's count instead of keeping a copy?

#[derive(Clone)]
pub enum Mode {
    Normal(normal::State),
    Insert(insert::State),
    Prompt(prompt::State),
    // TODO: Replace these with multi-key mappings in normal mode
    Seek(seek::State),
    Goto(goto::State),
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
        Self::Normal(normal::State::default())
    }
}
