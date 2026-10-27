pub mod command;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod replace;

#[derive(Clone, Default)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
    Prompt(prompt::State),
    Replace,
}

impl Mode {
    #[must_use]
    pub fn kind(&self) -> ModeKind {
        match self {
            Self::Normal => ModeKind::Normal,
            Self::Insert => ModeKind::Insert,
            Self::Prompt(_) => ModeKind::Prompt,
            Self::Replace => ModeKind::Replace,
        }
    }
}

#[derive(Clone, Copy)]
pub enum ModeKind {
    Normal,
    Insert,
    Prompt,
    // TODO: `Command`?
    Replace,
}
