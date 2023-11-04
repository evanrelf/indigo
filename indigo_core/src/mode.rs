#[derive(Debug)]
pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

#[derive(Debug, Default)]
pub struct NormalMode {
    count: usize,
}

#[derive(Debug, Default)]
pub struct InsertMode {}
