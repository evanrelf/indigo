// TODO: Stop allowing dead code
#![allow(dead_code)]

mod buffer;
mod editor;
mod position;
mod selection;
mod terminal;

use crate::{editor::Editor, terminal::Terminal};
use std::{env, panic};

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
