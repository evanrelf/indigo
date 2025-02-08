pub enum Mode {
    Normal { count: usize },
    Insert,
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> usize {
        match self {
            Self::Normal { count } => *count,
            Self::Insert => 0,
        }
    }

    pub fn set_count(&mut self, count: usize) {
        let new_count = count;
        match self {
            Self::Normal { count } => *count = new_count,
            Self::Insert => {}
        }
    }
}
