mod command;
mod insert;
mod normal;

pub use crate::mode::{command::CommandMode, insert::InsertMode, normal::NormalMode};

#[derive(Clone, Debug)]
pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
    Command(CommandMode),
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

#[derive(Clone, Debug, Default)]
pub enum ModeKind {
    #[default]
    Normal,
    Insert,
    Command,
}
