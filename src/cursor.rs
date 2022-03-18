use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
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
