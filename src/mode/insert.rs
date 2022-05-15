use crate::{editor::Editor, key::Key, mode::Mode};

#[derive(Default)]
pub struct InsertMode {}

impl Mode for InsertMode {
    fn mode_name(&self) -> &'static str {
        "insert"
    }

    fn handle_key(&mut self, _editor: &mut Editor, _key: Key) -> Result<(), anyhow::Error> {
        todo!()
    }
}
