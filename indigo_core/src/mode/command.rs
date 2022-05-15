use crate::{command_line::CommandLine, editor::Editor, key::Key, mode::Mode};

#[derive(Default)]
pub struct CommandMode {
    pub command_line: CommandLine,
}

impl Mode for CommandMode {
    fn mode_name(&self) -> &'static str {
        "command"
    }

    fn handle_key(&mut self, _editor: &mut Editor, _key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}
