// TODO: Stop allowing dead code
#![allow(dead_code)]

mod buffer;
mod cursor;
mod editor;
mod operand;
mod selection;
mod terminal;

use crate::{editor::Editor, terminal::with_terminal};

fn main() {
    let mut editor = Editor::new();

    if let Some(path) = std::env::args().nth(1) {
        editor.load_file(path);
    }

    with_terminal(|terminal| editor.run(terminal));
}
