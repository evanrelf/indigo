use crate::{editor::Editor, mode::Mode};
use crossterm::event::KeyEvent;

#[derive(Default)]
pub struct InsertMode {}

impl Mode for InsertMode {
    fn mode_name(&self) -> &'static str {
        "insert"
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
