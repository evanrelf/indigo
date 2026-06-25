pub mod command;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod replace;
pub mod seek;

#[derive(Clone, Default)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
    Prompt(prompt::State),
    // TODO: Replace this with multi-key mappings in normal mode
    Seek(seek::State),
    Replace,
}

impl Mode {
    #[must_use]
    pub fn kind(&self) -> ModeKind {
        match self {
            Self::Normal => ModeKind::Normal,
            Self::Insert => ModeKind::Insert,
            Self::Prompt(_) => ModeKind::Prompt,
            Self::Seek(_) => ModeKind::Seek,
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
    Seek,
    Replace,
}
