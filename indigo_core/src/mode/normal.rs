use crate::{editor::Editor, key::Key, mode::Mode};

#[derive(Default)]
pub struct NormalMode {
    pub count: usize,
}

impl Mode for NormalMode {
    fn mode_name(&self) -> &'static str {
        "normal"
    }

    fn handle_key(&mut self, _editor: &mut Editor, _key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}
