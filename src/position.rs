use ropey::Rope;
use std::fmt::Display;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) old_column: Option<usize>,
}

impl Position {
    pub(crate) fn new(line: usize, column: usize) -> Position {
        Position {
            line,
            column,
            old_column: None,
        }
    }

    pub(crate) fn move_up(self, distance: usize) -> Position {
        Position {
            line: self.line.saturating_sub(distance),
            ..self
        }
    }

    pub(crate) fn move_down(self, distance: usize) -> Position {
        Position {
            line: self.line + distance,
            ..self
        }
    }

    pub(crate) fn move_left(self, distance: usize) -> Position {
        Position {
            column: self.column.saturating_sub(distance),
            ..self
        }
    }

    pub(crate) fn move_right(self, distance: usize) -> Position {
        Position {
            column: self.column + distance,
            ..self
        }
    }
}

// With rope
impl Position {
    pub(crate) fn to_index(self, rope: &Rope) -> Option<usize> {
        let Position { line, column, .. } = self;
        let line_index = rope.try_line_to_char(line).ok()?;
        let line_length = rope.get_line(line)?.len_chars();
        if line_length > column {
            Some(line_index + column)
        } else {
            None
        }
    }

    pub(crate) fn from_index(rope: &Rope, index: usize) -> Option<Position> {
        let line = rope.try_char_to_line(index).ok()?;
        let column = index - rope.try_line_to_char(line).ok()?;
        Some(Position {
            line,
            column,
            old_column: None,
        })
    }

    pub(crate) fn is_valid(&self, rope: &Rope) -> bool {
        self.to_index(rope).is_some()
    }

    pub(crate) fn corrected(&self, rope: &Rope) -> Option<Position> {
        let Position {
            line,
            column,
            old_column,
        } = *self;
        let line_length = rope.get_line(line)?.len_chars();

        if line_length == 0 {
            return None;
        }

        match old_column {
            Some(old_column) if line_length > old_column => Some(Position {
                column: old_column,
                old_column: None,
                ..*self
            }),
            Some(_) => Some(Position {
                column: line_length - 1,
                ..*self
            }),
            None if line_length > column => Some(Position { ..*self }),
            None => Some(Position {
                column: line_length - 1,
                old_column: Some(column),
                ..*self
            }),
        }
    }
}

impl Display for Position {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line, self.column)
    }
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Position {
        Position {
            line,
            column,
            old_column: None,
        }
    }
}

#[test]
fn test_partial_ord() {
    assert!(Position::from((0, 0)) < Position::from((1, 0)));
    assert!(Position::from((0, 99)) < Position::from((1, 0)));
    assert!(Position::from((1, 0)) < Position::from((1, 1)));
}
