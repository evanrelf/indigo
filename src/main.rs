mod editor;
mod terminal;

use crate::editor::Editor;
use crate::terminal::Terminal;
use crossterm::Result;
use std::panic;

fn main() -> Result<()> {
    panic::set_hook(Box::new(|panic_info| {
        Terminal::exit();
        eprintln!("{}", panic_info);
    }));

    Editor::new().run();

    Ok(())
}
