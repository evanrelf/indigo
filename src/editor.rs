use crate::buffer::Buffer;
use crate::terminal::Terminal;
use crossterm::{cursor, event, style, terminal};
use std::cmp::min;
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
                    KeyCode::Char('h') => self.buffers[self.buffer_index].scroll_left(1),
                    KeyCode::Char('j') => self.buffers[self.buffer_index].scroll_down(1),
                    KeyCode::Char('k') => self.buffers[self.buffer_index].scroll_up(1),
                    KeyCode::Char('l') => self.buffers[self.buffer_index].scroll_right(1),
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

        for _ in 0..self.viewport.lines {
            Terminal::queue(style::Print("~"));
            Terminal::queue(cursor::MoveToNextLine(1));
        }

        let buffer = &self.buffers[self.buffer_index];

        let rope_slice = {
            let start = buffer.contents.line_to_char(buffer.lines_offset());
            let end = buffer.contents.line_to_char(min(
                start + (self.viewport.lines as usize),
                buffer.contents.len_lines(),
            ));
            buffer.contents.slice(start..end)
        };

        Terminal::queue(cursor::MoveTo(0, 0));

        for line in rope_slice.lines() {
            Terminal::queue(terminal::Clear(terminal::ClearType::CurrentLine));

            if line.len_chars().saturating_sub(buffer.columns_offset()) > 0 {
                let line2 = line.slice(buffer.columns_offset()..);
                Terminal::queue(style::Print(line2));
                Terminal::queue(cursor::MoveToColumn(0));
            } else {
                Terminal::queue(cursor::MoveToNextLine(1));
            }
        }

        Terminal::flush();
    }
}
