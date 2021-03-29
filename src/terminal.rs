use crossterm::{cursor, terminal, ExecutableCommand, QueueableCommand};
use std::io::{stdout, Write};

pub struct Terminal {}

impl Terminal {
    pub fn enter() {
        terminal::enable_raw_mode().unwrap();
        stdout().execute(terminal::EnterAlternateScreen).unwrap();
    }

    pub fn exit() {
        stdout().queue(cursor::Show).unwrap();
        stdout().queue(terminal::LeaveAlternateScreen).unwrap();
        stdout().flush().unwrap();
        terminal::disable_raw_mode().unwrap();
    }
}
