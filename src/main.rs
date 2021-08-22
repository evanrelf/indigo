mod buffer;
mod editor;
mod terminal;

use crate::editor::Editor;
use std::env;

fn main() {
    let mut editor = Editor::new();

    if let Some(path) = env::args().nth(1) {
        editor.load_file(path);
    }

    editor.run();
}
