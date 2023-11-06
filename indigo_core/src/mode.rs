use ropey::Rope;

#[derive(Clone, Debug)]
pub enum Mode {
    Normal(NormalMode),
    Insert(InsertMode),
    Command(CommandMode),
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

#[derive(Clone, Debug, Default)]
pub struct CommandMode {
    command: Rope,
    cursor: usize,
}
