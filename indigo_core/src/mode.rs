use crate::{command_line::CommandLine, editor::Editor, key::Key};
use downcast_rs::{impl_downcast, Downcast};

pub trait Mode: Downcast {
    fn mode_name(&self) -> &'static str;

    fn handle_key(&mut self, editor: &mut Editor, key: Key) -> Result<(), anyhow::Error>;
}
impl_downcast!(Mode);

#[derive(Default)]
pub struct NormalMode {
    pub count: usize,
}

impl Mode for NormalMode {
    fn mode_name(&self) -> &'static str {
        "normal"
    }

    fn handle_key(&mut self, editor: &mut Editor, key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}

#[derive(Default)]
pub struct CommandMode {
    pub command_line: CommandLine,
}

impl Mode for CommandMode {
    fn mode_name(&self) -> &'static str {
        "command"
    }

    fn handle_key(&mut self, editor: &mut Editor, key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}

#[derive(Default)]
pub struct InsertMode {}

impl Mode for InsertMode {
    fn mode_name(&self) -> &'static str {
        "insert"
    }

    fn handle_key(&mut self, editor: &mut Editor, key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}
