use std::num::NonZeroUsize;

pub struct Position {
    line: NonZeroUsize,
    column: NonZeroUsize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(1).unwrap(),
        }
    }
}
