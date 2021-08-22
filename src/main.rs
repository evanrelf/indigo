mod buffer;
mod editor;
mod terminal;

use crate::editor::Editor;
use crate::terminal::Terminal;
use std::env;
use std::panic;

fn main() {
    panic::set_hook(Box::new(|panic_info| {
        Terminal::exit();
        eprintln!("{}", panic_info);
    }));

    let mut editor = Editor::new();

    if let Some(path) = env::args().nth(1) {
        editor.load_file(path);
    }

    editor.run();
}
