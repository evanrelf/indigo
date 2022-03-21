use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Cursor {
    pub line: usize,
    pub column: usize,
    pub target_column: Option<usize>,
}

impl Cursor {
    pub fn new(line: usize, column: usize) -> Self {
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
    fn from((line, column): (usize, usize)) -> Self {
        Cursor {
            line,
            column,
            target_column: None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_partial_ord() {
        assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
        assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
        assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
    }
}
