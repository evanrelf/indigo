use crate::command_line::CommandLine;

pub enum Mode {
    Normal(NormalMode),
    Command(CommandMode),
    Insert(InsertMode),
}

#[derive(Default)]
pub struct NormalMode {
    pub count: usize,
}

#[derive(Default)]
pub struct CommandMode {
    pub command_line: CommandLine,
}

#[derive(Default)]
pub struct InsertMode {}
