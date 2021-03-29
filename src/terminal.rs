use crossterm::{cursor, event, style, terminal, ExecutableCommand, QueueableCommand};
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

    pub fn enable_mouse_capture() {
        stdout().queue(event::EnableMouseCapture).unwrap();
    }

    pub fn disable_mouse_capture() {
        stdout().queue(event::DisableMouseCapture).unwrap();
    }

    pub fn clear() {
        stdout()
            .queue(terminal::Clear(terminal::ClearType::All))
            .unwrap();
    }

    pub fn hide_cursor() {
        stdout().queue(cursor::Hide).unwrap();
    }

    pub fn move_cursor_to(x: u16, y: u16) {
        stdout().queue(cursor::MoveTo(x, y)).unwrap();
    }

    pub fn set_title(title: &str) {
        stdout().queue(terminal::SetTitle(title)).unwrap();
    }

    pub fn print(message: &str) {
        stdout().queue(style::Print(message)).unwrap();
    }

    pub fn print_styled(message: style::StyledContent<String>) {
        stdout().queue(style::PrintStyledContent(message)).unwrap();
    }

    pub fn flush() {
        stdout().flush().unwrap();
    }

    pub fn size() -> (u16, u16) {
        terminal::size().unwrap()
    }
}
