use ropey::Rope;
use std::num::NonZeroUsize;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

impl Position {
    #[must_use]
    pub fn is_valid(&self, rope: &Rope) -> bool {
        let rope_line = self.line.get() - 1;

        // Assert line is valid
        if rope.len_lines() <= rope_line {
            return false;
        }

        let rope_column = self.column.get() - 1;

        // Assert column is valid
        if rope.line(rope_line).len_chars() <= rope_column {
            return false;
        }

        true
    }

    #[must_use]
    pub fn corrected(&self, rope: &Rope) -> Self {
        let (index, _) = self.to_rope_index(rope);
        Self::from_rope_index(rope, index).0
    }

    #[must_use]
    pub fn to_rope_index(&self, rope: &Rope) -> (usize, bool) {
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

        (rope.line_to_char(line) + column, corrected)
    }

    #[must_use]
    pub fn from_rope_index(rope: &Rope, index: usize) -> (Self, bool) {
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

        (position, corrected)
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
    fn test_conversions() {
        let rope = Rope::from("Hello\nworld\n");

        let position = Position::try_from((1, 1)).unwrap();
        let rope_index = 0;
        assert_eq!(position.to_rope_index(&rope), (rope_index, false));
        assert_eq!(
            Position::from_rope_index(&rope, rope_index),
            (position, false)
        );

        let position = Position::try_from((2, 1)).unwrap();
        let rope_index = 6;
        assert_eq!(position.to_rope_index(&rope), (rope_index, false));
        assert_eq!(
            Position::from_rope_index(&rope, rope_index),
            (position, false)
        );

        let position = Position::try_from((99, 99)).unwrap();
        assert!(!position.is_valid(&rope));
        assert_eq!(
            Position::from_rope_index(&rope, 999),
            (Position::try_from((2, 6)).unwrap(), true)
        );

        let position = Position::try_from((99, 99)).unwrap();
        let rope_index = 11;
        assert_eq!(position.to_rope_index(&rope), (rope_index, true));
    }
}
