pub enum Mode {
    Normal(NormalMode),
    Insert,
}

pub struct NormalMode {
    pub count: usize,
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> usize {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Insert => 0,
        }
    }

    pub fn set_count(&mut self, count: usize) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Insert => {}
        }
    }
}
