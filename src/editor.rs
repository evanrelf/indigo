use crate::buffer::Buffer;
use crate::terminal::Terminal;
use crossterm::{cursor, event, style, terminal};
use std::path::Path;

pub struct Editor {
    quit: bool,

    buffers: Vec<Buffer>,
    buffer_index: usize,

    viewport_lines: u16,
    viewport_columns: u16,
}

impl Editor {
    pub fn new() -> Editor {
        let (viewport_columns, viewport_lines) = Terminal::size();

        Editor {
            quit: false,

            buffers: vec![Buffer::new()],
            buffer_index: 0,

            viewport_lines,
            viewport_columns,
        }
    }

    pub fn load_file<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        self.buffers.push(Buffer::from_file(path));
        self.buffer_index += 1;
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

    fn render(&self) {
        Terminal::queue(terminal::Clear(terminal::ClearType::All));

        self.render_tildes();
        self.render_buffer();

        Terminal::flush();
    }

    fn render_tildes(&self) {
        Terminal::queue(cursor::MoveTo(0, 0));

        for _ in 0..self.viewport_lines {
            Terminal::queue(style::Print("~"));
            Terminal::queue(cursor::MoveToNextLine(1));
        }
    }

    fn render_buffer(&self) {
        let buffer = &self.buffers[self.buffer_index];

        let viewport_slice = {
            let viewport_start_line = buffer.viewport_lines_offset;
            let viewport_end_line = viewport_start_line + (self.viewport_lines as usize);
            let buffer_end_line = buffer.contents.len_lines();
            let start = buffer.contents.line_to_char(viewport_start_line);
            if buffer_end_line <= viewport_end_line {
                buffer.contents.slice(start..)
            } else {
                let end = buffer.contents.line_to_char(viewport_end_line);
                buffer.contents.slice(start..end)
            }
        };

        for (i, line) in viewport_slice.lines().enumerate() {
            if line
                .len_chars()
                .saturating_sub(buffer.viewport_columns_offset)
                > 0
            {
                let line = line.slice(buffer.viewport_columns_offset..);
                Terminal::queue(cursor::MoveTo(0, i as u16));
                Terminal::queue(terminal::Clear(terminal::ClearType::CurrentLine));
                Terminal::queue(style::Print(String::from(line).trim_end()));
            }
        }
    }

    fn handle_event(&mut self) {
        use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
        match event::read().unwrap() {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    match code {
                        KeyCode::Char('c') => self.quit = true,
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        KeyCode::Up => self.buffers[self.buffer_index].scroll_up(1),
                        KeyCode::Down => self.buffers[self.buffer_index].scroll_down(1),
                        KeyCode::Left => self.buffers[self.buffer_index].scroll_left(1),
                        KeyCode::Right => self.buffers[self.buffer_index].scroll_right(1),
                        _ => (),
                    }
                }
            }
            Event::Resize(columns, lines) => {
                self.viewport_lines = lines;
                self.viewport_columns = columns;
            }
            _ => (),
        }
    }
}
