use ropey::Rope;
use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Cursor {
    pub line: usize,
    pub column: usize,
    pub target_column: Option<usize>,
}

impl Cursor {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }

    pub fn to_rope_index(&self, rope: &Rope) -> Option<usize> {
        let bol_char_index = rope.try_line_to_char(self.line).ok()?;
        let line_length = rope.get_line(self.line)?.len_chars();
        if line_length < self.column {
            return None;
        }
        Some(bol_char_index + self.column)
    }

    #[allow(unused_variables)]
    pub fn to_rope_index_lossy(&self, rope: &Rope) -> usize {
        todo!()
    }

    pub fn from_rope_index(rope: &Rope, index: usize) -> Option<Self> {
        let line = rope.try_char_to_line(index).ok()?;
        let column = index - rope.try_line_to_char(line).ok()?;
        Some(Self {
            line,
            column,
            target_column: None,
        })
    }

    #[allow(unused_variables)]
    pub fn from_rope_index_lossy(rope: &Rope, index: usize) -> Self {
        todo!()
    }
}

impl Display for Cursor {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl From<(usize, usize)> for Cursor {
    fn from((line, column): (usize, usize)) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }
}
