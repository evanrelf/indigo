use crossterm::{cursor, terminal, Command, ExecutableCommand, QueueableCommand};
use std::io::{stdout, Write};

pub(crate) struct Terminal {}

impl Terminal {
    pub(crate) fn enter() {
        terminal::enable_raw_mode().unwrap();
        stdout().execute(terminal::EnterAlternateScreen).unwrap();
    }

    pub(crate) fn exit() {
        let mut stdout = stdout();
        stdout.queue(cursor::Show).unwrap();
        stdout.queue(terminal::LeaveAlternateScreen).unwrap();
        stdout.flush().unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    pub(crate) fn execute<C>(command: C)
    where
        C: Command,
    {
        stdout().execute(command).unwrap();
    }

    pub(crate) fn queue<C>(command: C)
    where
        C: Command,
    {
        stdout().queue(command).unwrap();
    }

    pub(crate) fn flush() {
        stdout().flush().unwrap();
    }

    pub(crate) fn size() -> (u16, u16) {
        terminal::size().unwrap()
    }
}
