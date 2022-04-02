use ropey::Rope;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn to_rope_index(&self, rope: &Rope) -> Option<usize> {
        // Assert line is valid
        if rope.len_lines() <= self.line {
            return None;
        }

        // Assert column is valid
        if rope.line(self.line).len_chars() <= self.column {
            return None;
        }

        Some(rope.line_to_char(self.line) + self.column)
    }

    #[allow(unused_variables)]
    pub fn to_rope_index_lossy(&self, rope: &Rope) -> usize {
        todo!()
    }

    pub fn from_rope_index(rope: &Rope, index: usize) -> Option<Self> {
        // Assert index is valid
        if rope.len_chars() <= index {
            return None;
        }

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        Some(Self { line, column })
    }

    #[allow(unused_variables)]
    pub fn from_rope_index_lossy(rope: &Rope, index: usize) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rope_index_roundtrip() {
        let rope = Rope::from_str("hello\nworld\n!\n");

        let case = |line: usize, column: usize| {
            let position = Position { line, column };
            assert_eq!(
                Some(position.clone()),
                position
                    .to_rope_index(&rope)
                    .and_then(|index| Position::from_rope_index(&rope, index)),
            );
        };

        case(0, 0);
        case(1, 4);
        case(1, 5);
        case(2, 0);
    }
}
