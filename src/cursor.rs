use std::fmt::Display;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Cursor {
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) target_column: Option<usize>,
}

impl Cursor {
    pub(crate) fn new(line: usize, column: usize) -> Cursor {
        Cursor {
            line,
            column,
            target_column: None,
        }
    }

    pub(crate) fn move_up(self, distance: usize) -> Cursor {
        Cursor {
            line: self.line.saturating_sub(distance),
            ..self
        }
    }

    pub(crate) fn move_down(self, distance: usize) -> Cursor {
        Cursor {
            line: self.line + distance,
            ..self
        }
    }

    pub(crate) fn move_left(self, distance: usize) -> Cursor {
        Cursor {
            column: self.column.saturating_sub(distance),
            ..self
        }
    }

    pub(crate) fn move_right(self, distance: usize) -> Cursor {
        Cursor {
            column: self.column + distance,
            ..self
        }
    }
}

impl Display for Cursor {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line, self.column)
    }
}

impl From<(usize, usize)> for Cursor {
    fn from((line, column): (usize, usize)) -> Cursor {
        Cursor {
            line,
            column,
            target_column: None,
        }
    }
}

#[test]
fn test_partial_ord() {
    assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
    assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
    assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
}
