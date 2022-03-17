use ropey::Rope;
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

// With rope
impl Cursor {
    pub(crate) fn to_index(self, rope: &Rope) -> Option<usize> {
        let Cursor { line, column, .. } = self;
        let line_index = rope.try_line_to_char(line).ok()?;
        let line_length = rope.get_line(line)?.len_chars();
        if line_length > column {
            Some(line_index + column)
        } else {
            None
        }
    }

    pub(crate) fn from_index(rope: &Rope, index: usize) -> Option<Cursor> {
        let line = rope.try_char_to_line(index).ok()?;
        let column = index - rope.try_line_to_char(line).ok()?;
        Some(Cursor {
            line,
            column,
            target_column: None,
        })
    }

    pub(crate) fn is_valid(&self, rope: &Rope) -> bool {
        self.to_index(rope).is_some()
    }

    pub(crate) fn corrected(&self, rope: &Rope) -> Option<Cursor> {
        let Cursor {
            line,
            column,
            target_column,
        } = *self;
        let line_length = rope.get_line(line)?.len_chars();

        if line_length == 0 {
            return None;
        }

        match target_column {
            Some(target_column) if line_length > target_column => Some(Cursor {
                column: target_column,
                target_column: None,
                ..*self
            }),
            Some(_) => Some(Cursor {
                column: line_length - 1,
                ..*self
            }),
            None if line_length > column => Some(Cursor { ..*self }),
            None => Some(Cursor {
                column: line_length - 1,
                target_column: Some(column),
                ..*self
            }),
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
