use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::{cursor, event, style, terminal, ExecutableCommand, QueueableCommand, Result};
use std::io::{stdout, Write};

struct Terminal {}

impl Terminal {
    fn enter() {
        terminal::enable_raw_mode().unwrap();
        stdout().execute(terminal::EnterAlternateScreen).unwrap();
    }

    fn exit() {
        stdout().queue(cursor::Show).unwrap();
        stdout().queue(terminal::LeaveAlternateScreen).unwrap();
        stdout().flush().unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    fn clear() {
        stdout()
            .queue(terminal::Clear(terminal::ClearType::All))
            .unwrap();
    }

    fn hide_cursor() {
        stdout().queue(cursor::Hide).unwrap();
    }

    fn show_cursor() {
        stdout().queue(cursor::Show).unwrap();
    }

    fn move_cursor_to(x: u16, y: u16) {
        stdout().queue(cursor::MoveTo(x, y)).unwrap();
    }

    fn set_title(title: &str) {
        stdout().queue(terminal::SetTitle(title)).unwrap();
    }

    fn print(message: &str) {
        stdout().queue(style::Print(message)).unwrap();
    }

    fn flush() {
        stdout().flush().unwrap();
    }
}

struct Editor {
    quit: bool,
    buffer: String,
}

impl Editor {
    fn new() -> Editor {
        Editor {
            quit: false,
            buffer: String::from(""),
        }
    }

    fn handle_event(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) => self.handle_key_event(key_event),
            Event::Mouse(mouse_event) => self.handle_mouse_event(mouse_event),
            Event::Resize(width, height) => self.handle_resize_event(width, height),
        }
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) -> Result<()> {
        let KeyEvent { code, modifiers } = key_event;

        if modifiers == KeyModifiers::NONE {
            match code {
                KeyCode::Char(c) => {
                    self.buffer.push(c);
                }
                KeyCode::Enter => {
                    self.buffer.push_str("\n\r");
                }
                KeyCode::Backspace => {
                    if let Some('\r') = self.buffer.pop() {
                        self.buffer.pop();
                    }
                }
                _ => (),
            }
        }

        if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('c') => self.quit = true,
                _ => (),
            }
        }

        Ok(())
    }

    fn handle_mouse_event(&mut self, mouse_event: MouseEvent) -> Result<()> {
        match mouse_event {
            _ => Ok(()),
        }
    }

    fn handle_resize_event(&mut self, _width: u16, _height: u16) -> Result<()> {
        Ok(())
    }

    fn render(&mut self) {
        Terminal::clear();
        Terminal::move_cursor_to(0, 0);
        Terminal::print(&self.buffer);
        Terminal::flush();
    }
}

fn main() -> Result<()> {
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
