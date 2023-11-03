pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

#[derive(Default)]
pub struct NormalMode {
    count: usize,
}

#[derive(Default)]
pub struct InsertMode {}
