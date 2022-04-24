use std::num::NonZeroUsize;

#[derive(Clone, Copy)]
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

impl From<(NonZeroUsize, NonZeroUsize)> for Position {
    fn from((line, column): (NonZeroUsize, NonZeroUsize)) -> Self {
        Self { line, column }
    }
}

impl TryFrom<(usize, usize)> for Position {
    type Error = ();

    fn try_from((line, column): (usize, usize)) -> Result<Self, Self::Error> {
        Ok(Self {
            line: NonZeroUsize::new(line).ok_or(())?,
            column: NonZeroUsize::new(column).ok_or(())?,
        })
    }
}
