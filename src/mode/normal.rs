use crate::{editor::Editor, mode::Mode};
use crossterm::event::KeyEvent;

#[derive(Default)]
pub struct NormalMode {
    pub count: usize,
}

impl Mode for NormalMode {
    fn mode_name(&self) -> &'static str {
        "normal"
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
