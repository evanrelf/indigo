use ropey::Rope;

#[derive(Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_rope_index(index: usize, rope: Rope) -> Self {
        todo!()
    }

    pub fn to_rope_index(self, rope: Rope) -> usize {
        todo!()
    }
}
