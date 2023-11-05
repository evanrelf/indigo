#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(NormalMode::default())
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct NormalMode {
    count: usize,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct InsertMode {}
