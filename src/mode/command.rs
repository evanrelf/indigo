use crate::{command_line::CommandLine, editor::Editor, mode::Mode};
use crossterm::event::KeyEvent;

#[derive(Default)]
pub struct CommandMode {
    pub command_line: CommandLine,
}

impl Mode for CommandMode {
    fn mode_name(&self) -> &'static str {
        "command"
    }

    fn handle_key(
        &mut self,
        _editor: &mut Editor,
        _key_event: KeyEvent,
    ) -> Result<bool, anyhow::Error> {
        // TODO
        Ok(false)
    }
}
