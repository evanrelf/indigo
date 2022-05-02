use crate::validate::{Valid, Validate};
use ropey::Rope;
use std::num::NonZeroUsize;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

impl Position {
    #[must_use]
    pub fn corrected<'rope>(&self, rope: &'rope Rope) -> Valid<'rope, Self> {
        let index = *self.to_rope_index(rope).0;
        Self::from_rope_index(rope, index).0
    }

    #[must_use]
    pub fn to_rope_index<'rope>(&self, rope: &'rope Rope) -> (Valid<'rope, usize>, bool) {
        let mut corrected = false;

        let rope_line = self.line.get() - 1;

        // Get valid line
        let lines = rope.len_lines();
        let line = if lines <= rope_line {
            // When line goes beyond end of rope, use last line
            // Subtracting 1 to convert to zero-based index, subtracting another 1 to remove ropey's
            // mysterious empty final line
            corrected = true;
            lines.saturating_sub(2)
        } else {
            rope_line
        };

        let rope_column = self.column.get() - 1;

        // Get valid column
        let columns = rope.line(line).len_chars();
        let column = if columns <= rope_column {
            // When column goes beyond end of line, use last column
            corrected = true;
            columns.saturating_sub(1)
        } else {
            rope_column
        };

        let index = (rope.line_to_char(line) + column).valid_for(rope).unwrap();

        (index, corrected)
    }

    #[must_use]
    pub fn from_rope_index(rope: &Rope, index: usize) -> (Valid<'_, Self>, bool) {
        let mut corrected = false;

        // Get valid index
        let index = if rope.len_chars() <= index {
            // When index goes beyond end of rope, use last index
            corrected = true;
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let rope_line = rope.char_to_line(index);

        let rope_column = index - rope.line_to_char(rope_line);

        let position = Self {
            line: NonZeroUsize::new(rope_line + 1).unwrap(),
            column: NonZeroUsize::new(rope_column + 1).unwrap(),
        };

        (position.valid_for(rope).unwrap(), corrected)
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(1).unwrap(),
        }
    }
}

impl Validate<Rope> for Position {
    #[must_use]
    fn is_valid(&self, rope: Option<&Rope>) -> bool {
        if let Some(rope) = rope {
            let rope_line = self.line.get() - 1;

            // Assert line is valid
            if rope.len_lines().saturating_sub(1) <= rope_line {
                return false;
            }

            let rope_column = self.column.get() - 1;

            // Assert column is valid
            if rope.line(rope_line).len_chars() <= rope_column {
                return false;
            }

            true
        } else {
            // TODO
            false
        }
    }
}

impl Validate<Rope> for usize {
    #[must_use]
    fn is_valid(&self, rope: Option<&Rope>) -> bool {
        if let Some(rope) = rope {
            rope.len_chars() > *self
        } else {
            // TODO
            false
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(usize, Position)> = vec![
            (0, (1, 1)),
            (1, (1, 2)),
            (5, (1, 6)),
            (6, (2, 1)),
            (11, (2, 6)),
        ]
        .into_iter()
        .map(|(i, p)| (i, p.try_into().unwrap()))
        .collect();

        for (index, position) in cases {
            let (actual_index, index_corrected) = position.to_rope_index(rope);
            let (actual_position, position_corrected) = Position::from_rope_index(rope, index);
            assert!(!index_corrected && !position_corrected);
            assert_eq!(index, *actual_index);
            assert_eq!(position, *actual_position);
        }
    }

    #[test]
    fn test_from_rope_index() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(usize, Position, bool)> = vec![
            (0, (1, 1), false),
            (1, (1, 2), false),
            (5, (1, 6), false),
            (6, (2, 1), false),
            (11, (2, 6), false),
            (12, (2, 6), true),
            (999, (2, 6), true),
        ]
        .into_iter()
        .map(|(i, p, c)| (i, p.try_into().unwrap(), c))
        .collect();

        for (index, position, corrected) in cases {
            let (actual_position, actual_corrected) = Position::from_rope_index(rope, index);
            assert_eq!(position, *actual_position);
            assert_eq!(corrected, actual_corrected);
        }
    }

    #[test]
    fn test_to_rope_index() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(Position, usize, bool)> = vec![
            ((1, 1), 0, false),
            ((1, 2), 1, false),
            ((1, 6), 5, false),
            ((2, 1), 6, false),
            ((2, 6), 11, false),
            ((2, 7), 11, true),
            ((99, 99), 11, true),
        ]
        .into_iter()
        .map(|(p, i, c)| (p.try_into().unwrap(), i, c))
        .collect();

        for (position, index, corrected) in cases {
            let (actual_index, actual_corrected) = position.to_rope_index(rope);
            assert_eq!(index, *actual_index);
            assert_eq!(corrected, actual_corrected);
        }
    }
}
