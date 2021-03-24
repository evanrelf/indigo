use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::{cursor, event, style, terminal, ExecutableCommand, Result};
use std::io::stdout;

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

struct Editor {
    quit: bool,
}

impl Editor {
    fn new() -> Editor {
        Editor { quit: false }
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
}

fn main() -> Result<()> {
    Terminal::enter();

    Terminal::set_title("idg");

    Terminal::clear();
    Terminal::move_cursor_to(0, 0);

    let mut editor = Editor::new();

    while !editor.quit {
        editor.handle_event()?;
    }

    Terminal::exit();

    Ok(())
}
