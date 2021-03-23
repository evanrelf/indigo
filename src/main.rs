use crossterm::{cursor, style, terminal, ExecutableCommand, Result};
use std::io::stdout;
use std::thread::sleep;
use std::time::Duration;

struct Terminal {}

impl Terminal {
    fn enter() {
        terminal::enable_raw_mode().unwrap();
        stdout().execute(terminal::EnterAlternateScreen).unwrap();
    }

    fn exit() {
        stdout().execute(terminal::LeaveAlternateScreen).unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    fn clear() {
        stdout()
            .execute(terminal::Clear(terminal::ClearType::All))
            .unwrap();
    }

    fn move_cursor_to(x: u16, y: u16) {
        stdout().execute(cursor::MoveTo(x, y)).unwrap();
    }

    fn set_title(title: &str) {
        stdout().execute(terminal::SetTitle(title)).unwrap();
    }

    fn print(message: &str) {
        stdout().execute(style::Print(message)).unwrap();
    }
}

fn main() -> Result<()> {
    Terminal::enter();

    Terminal::set_title("idg");

    Terminal::clear();
    Terminal::move_cursor_to(0, 0);
    Terminal::print("Hello, world!");

    sleep(Duration::new(1, 0));

    Terminal::exit();

    Ok(())
}
