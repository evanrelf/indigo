mod buffer;
mod editor;
mod selection;
mod terminal;

use crate::editor::Editor;
use crate::terminal::Terminal;
use crossterm::Result;
use std::env;
use std::panic;

fn main() -> Result<()> {
    panic::set_hook(Box::new(|panic_info| {
        Terminal::exit();
        eprintln!("{}", panic_info);
    }));

    if let Some(path) = env::args().nth(1) {
        Editor::from_file(path).run();
    } else {
        Editor::empty().run();
    }

    Ok(())
}
