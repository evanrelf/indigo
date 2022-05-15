use crate::command_line::CommandLine;

pub enum Mode {
    Normal { count: usize },
    Command { command_line: CommandLine },
    Insert,
}

impl Mode {
    #[must_use]
    pub fn normal() -> Self {
        Self::Normal { count: 0 }
    }

    #[must_use]
    pub fn command() -> Self {
        Self::Command {
            command_line: CommandLine::default(),
        }
    }

    #[must_use]
    pub fn insert() -> Self {
        Self::Insert
    }
}
