use crate::{command_line::CommandLine, key::KeyMap};
use downcast_rs::{impl_downcast, Downcast};

pub trait Mode: Downcast {
    fn mode_name(&self) -> &'static str;
}
impl_downcast!(Mode);

#[derive(Default)]
pub struct NormalMode {
    pub keymap: KeyMap,
    pub count: usize,
}

impl Mode for NormalMode {
    fn mode_name(&self) -> &'static str {
        "normal"
    }
}

#[derive(Default)]
pub struct CommandMode {
    pub keymap: KeyMap,
    pub command_line: CommandLine,
}

impl Mode for CommandMode {
    fn mode_name(&self) -> &'static str {
        "command"
    }
}

#[derive(Default)]
pub struct InsertMode {
    pub keymap: KeyMap,
}

impl Mode for InsertMode {
    fn mode_name(&self) -> &'static str {
        "insert"
    }
}
