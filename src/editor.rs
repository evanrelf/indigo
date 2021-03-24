use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::Result;

pub struct Editor {
    pub quit: bool,
    buffer: String,
}

impl Editor {
    pub fn new() -> Editor {
        Editor {
            quit: false,
            buffer: String::from(""),
        }
    }

    pub fn handle_event(&mut self) -> Result<()> {
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

    pub fn render(&mut self) {
        Terminal::clear();
        Terminal::move_cursor_to(0, 0);
        Terminal::print(&self.buffer);
        Terminal::flush();
    }
}
