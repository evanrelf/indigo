use crate::buffer::Buffer;
use crate::terminal::Terminal;
use crossterm::{cursor, event, style, terminal};
use std::fs::File;
use std::path::Path;

pub struct Viewport {
    lines: u16,
    columns: u16,
}

pub struct Editor {
    quit: bool,
    viewport: Viewport,
    buffer_index: usize,
    buffers: Vec<Buffer>,
}

impl Editor {
    pub fn new() -> Editor {
        let (columns, lines) = Terminal::size();

        Editor {
            quit: false,
            viewport: Viewport { lines, columns },
            buffer_index: 0,
            buffers: vec![Buffer::new()],
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

    pub fn load_file<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        self.buffers.push(Buffer::from_file(path));
        self.buffer_index += 1;
    }

    pub fn handle_event(&mut self) {
        use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
        match event::read().unwrap() {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;
                match code {
                    KeyCode::Char('c') if modifiers == KeyModifiers::CONTROL => self.quit = true,
                    _ => (),
                }
            }
            Event::Resize(columns, lines) => {
                self.viewport.lines = lines;
                self.viewport.columns = columns;
            }
            _ => (),
        }
    }

    pub fn render(&self) {
        Terminal::queue(terminal::Clear(terminal::ClearType::All));
        Terminal::queue(cursor::MoveTo(0, 0));

        for line in 0..self.viewport.lines {
            Terminal::queue(style::Print("~"));
            Terminal::queue(cursor::MoveToNextLine(1));
        }

        Terminal::flush();
    }
}
