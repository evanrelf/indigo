use ropey::Rope;
use std::num::NonZeroUsize;

#[derive(Clone, Copy)]
pub struct Position {
    line: NonZeroUsize,
    column: NonZeroUsize,
}

impl Position {
    #[must_use]
    pub fn to_rope_index(&self, rope: &Rope) -> Option<usize> {
        let rope_line = self.line.get() - 1;

        // Assert line is valid
        if rope.len_lines() <= rope_line {
            return None;
        }

        let rope_column = self.column.get() - 1;

        // Assert column is valid
        if rope.line(rope_line).len_chars() <= rope_column {
            return None;
        }

        Some(rope.line_to_char(rope_line) + rope_column)
    }

    #[must_use]
    pub fn to_rope_index_corrected(&self, rope: &Rope) -> usize {
        let rope_line = self.line.get() - 1;

        // Get valid line
        let lines = rope.len_lines();
        let line = if lines <= rope_line {
            // When line goes beyond end of rope, use last line
            // Subtracting 1 to convert to zero-based index, subtracting another 1 to remove ropey's
            // mysterious empty final line
            lines.saturating_sub(2)
        } else {
            rope_line
        };

        let rope_column = self.column.get() - 1;

        // Get valid column
        let columns = rope.line(line).len_chars();
        let column = if columns <= rope_column {
            // When column goes beyond end of line, use last column
            columns.saturating_sub(1)
        } else {
            rope_column
        };

        rope.line_to_char(line) + column
    }

    #[must_use]
    pub fn from_rope_index(rope: &Rope, index: usize) -> Option<Self> {
        // Assert index is valid
        if rope.len_chars() <= index {
            return None;
        }

        let rope_line = rope.char_to_line(index);

        let rope_column = index - rope.line_to_char(rope_line);

        Some(Self {
            line: NonZeroUsize::new(rope_line + 1).unwrap(),
            column: NonZeroUsize::new(rope_column + 1).unwrap(),
        })
    }

    #[must_use]
    pub fn from_rope_index_corrected(rope: &Rope, index: usize) -> Self {
        // Get valid index
        let index = if rope.len_chars() <= index {
            // When index goes beyond end of rope, use last index
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let rope_line = rope.char_to_line(index);

        let rope_column = index - rope.line_to_char(rope_line);

        Self {
            line: NonZeroUsize::new(rope_line + 1).unwrap(),
            column: NonZeroUsize::new(rope_column + 1).unwrap(),
        }
    }

    #[must_use]
    pub fn corrected(&self, rope: &Rope) -> Self {
        let index = self.to_rope_index_corrected(rope);
        Self::from_rope_index(rope, index).unwrap()
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
