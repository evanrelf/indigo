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

    pub fn to_rope_index(&self, rope: Rope) -> Conversion<usize> {
        if rope.len_chars() == 0 {
            return Conversion::Invalid;
        }

        let mut corrected = false;

        let lines = rope.len_lines().saturating_sub(1);
        let line = if lines <= self.line {
            // When line goes beyond end of rope, use last line
            corrected = true;
            lines
        } else {
            self.line
        };

        let columns = rope.line(line).len_chars();
        let column = if corrected {
            // When line was corrected, use last column
            columns
        } else if columns <= self.column {
            // When column goes beyond end of line, use last column
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conversion::Conversion;

    #[test]
    fn rope_length_empty() {
        assert_eq!(Rope::default(), Rope::from_str(""));
        assert_eq!(Rope::default().len_chars(), 0);
        assert_eq!(Rope::default().len_lines(), 1); // empty rope has 1 line
    }

    #[test]
    fn rope_length_single_line() {
        assert_eq!(Rope::from_str("x").len_chars(), 1);
        assert_eq!(Rope::from_str("x").len_lines(), 1); // no trailing newline == 1 line

        assert_eq!(Rope::from_str("\n").len_chars(), 1);
        assert_eq!(Rope::from_str("\n").len_lines(), 2);

        assert_eq!(Rope::from_str("x\n").len_chars(), 2);
        assert_eq!(Rope::from_str("x\n").len_lines(), 2); // trailing newline == 2 lines
    }

    #[test]
    fn rope_length_multiple_lines() {
        assert_eq!(Rope::from_str("x\ny\nz").len_chars(), 5);
        assert_eq!(Rope::from_str("x\ny\nz").len_lines(), 3);

        assert_eq!(Rope::from_str("x\ny\nz\n").len_chars(), 6);
        assert_eq!(Rope::from_str("x\ny\nz\n").len_lines(), 4); // line count == '\n' count + 1
    }

    #[test]
    fn to_rope_index_invalid() {
        let rope = Rope::default();
        let position = Position { line: 0, column: 0 };
        assert_eq!(position.to_rope_index(rope), Conversion::Invalid);
    }

    #[test]
    fn to_rope_index_corrected() {
        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 9, column: 0 };
        assert_eq!(position.to_rope_index(rope), Conversion::Corrected(7));

        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 9, column: 0 };
        assert_eq!(position.to_rope_index(rope), Conversion::Corrected(8));
    }

    #[test]
    fn to_rope_index_valid() {
        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 1, column: 3 };
        assert_eq!(position.to_rope_index(rope), Conversion::Valid(7));
    }
}
