use crate::buffer::Buffer;
use crate::terminal::Terminal;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::{cursor, event, style, terminal};

pub struct Editor {
    quit: bool,
    buffers: Vec<Buffer>,
}

impl Editor {
    pub fn new() -> Editor {
        Editor {
            quit: false,
            buffers: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        Terminal::enter();
        Terminal::execute(cursor::Hide);

        while !self.quit {
            self.render();
            self.handle_event();
        }

        Terminal::exit();
    }

    pub fn handle_event(&mut self) {
        match event::read().unwrap() {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;
                match code {
                    KeyCode::Char('c') if modifiers == KeyModifiers::CONTROL => self.quit = true,
                    _ => (),
                }
            }
            _ => (),
        }
    }

    pub fn render(&self) {
        Terminal::queue(terminal::Clear(terminal::ClearType::All));
        Terminal::queue(cursor::MoveTo(0, 0));
        Terminal::queue(style::Print("Hello, world!"));
        Terminal::flush();
    }
}
