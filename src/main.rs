mod buffer;
mod editor;
mod terminal;

use crate::terminal::Terminal;
use std::panic;

fn main() {
    panic::set_hook(Box::new(|panic_info| {
        Terminal::exit();
        eprintln!("{}", panic_info);
    }));
}
