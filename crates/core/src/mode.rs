use std::num::NonZeroUsize;

pub enum Mode {
    Normal(NormalMode),
    Insert,
}

pub struct NormalMode {
    pub count: NonZeroUsize,
}

impl Mode {
    #[must_use]
    pub fn count(&self) -> NonZeroUsize {
        match self {
            Self::Normal(normal_mode) => normal_mode.count,
            Self::Insert => NonZeroUsize::MIN,
        }
    }

    pub fn set_count(&mut self, count: NonZeroUsize) {
        match self {
            Self::Normal(normal_mode) => normal_mode.count = count,
            Self::Insert => {}
        }
    }
}
