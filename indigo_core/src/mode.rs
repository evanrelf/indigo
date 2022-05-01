pub enum Mode {
    Normal { count: usize },
    Command { command: String },
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
            command: String::new(),
        }
    }

    #[must_use]
    pub fn insert() -> Self {
        Self::Insert
    }
}
