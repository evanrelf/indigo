use crate::conversion::Conversion;
use ropey::Rope;

#[derive(Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_rope_index(index: usize, rope: Rope) -> Conversion<Self> {
        if rope.len_chars() == 0 {
            return Conversion::Invalid;
        }

        let mut corrected = false;

        let index = if rope.len_chars() <= index {
            corrected = true;
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        let position = Position { line, column };

        if corrected {
            Conversion::Corrected(position)
        } else {
            Conversion::Valid(position)
        }
    }

    pub fn to_rope_index(self, rope: Rope) -> Conversion<usize> {
        if rope.len_chars() == 0 {
            return Conversion::Invalid;
        }

        let mut corrected = false;

        let lines = rope.len_lines().saturating_sub(1);

        let line = if lines <= self.line {
            lines.saturating_sub(1)
        } else {
            self.line
        };

        let columns = rope.line(line).len_chars();

        let column = if columns <= self.column {
            corrected = true;
            columns.saturating_sub(1)
        } else {
            self.column
        };

        let index = column + rope.line_to_char(line);

        if corrected {
            Conversion::Corrected(index)
        } else {
            Conversion::Valid(index)
        }
    }
}
