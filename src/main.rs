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

    Terminal::enter();

    Terminal::set_title("idg");
    Terminal::hide_cursor();
    Terminal::flush();

    let mut editor = Editor::new();

    while !editor.quit {
        editor.render();
        editor.handle_event()?;
    }

    Terminal::exit();

    Ok(())
}
