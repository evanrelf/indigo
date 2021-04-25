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
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        KeyCode::Char('h') => self.buffers[self.buffer_index].scroll_left(1),
                        KeyCode::Char('j') => self.buffers[self.buffer_index].scroll_down(1),
                        KeyCode::Char('k') => self.buffers[self.buffer_index].scroll_up(1),
                        KeyCode::Char('l') => self.buffers[self.buffer_index].scroll_right(1),
                        _ => (),
                    }
                }
            }
            Event::Resize(columns, lines) => {
                self.viewport.lines = lines;
                self.viewport.columns = columns;
            }
            _ => (),
        }
    }

    fn render_tildes(&self) {
        Terminal::queue(cursor::MoveTo(0, 0));

        for _ in 0..self.viewport.lines {
            Terminal::queue(style::Print("~"));
            Terminal::queue(cursor::MoveToNextLine(1));
        }
    }

    fn render_buffer(&self) {
        let buffer = &self.buffers[self.buffer_index];

        let viewport_slice = {
            let start = buffer.contents.line_to_char(buffer.lines_offset());
            let end = buffer.contents.line_to_char(min(
                start + (self.viewport.lines as usize),
                buffer.contents.len_lines(),
            ));
            buffer.contents.slice(start..end)
        };

        Terminal::queue(cursor::MoveTo(0, 0));

        for line in viewport_slice.lines() {
            Terminal::queue(terminal::Clear(terminal::ClearType::CurrentLine));

            if line.len_chars().saturating_sub(buffer.columns_offset()) > 0 {
                let line2 = line.slice(buffer.columns_offset()..);
                Terminal::queue(style::Print(line2));
                Terminal::queue(cursor::MoveToColumn(0));
            } else {
                Terminal::queue(cursor::MoveToNextLine(1));
            }
        }
    }

    fn render_selections(&self) {
        let buffer = &self.buffers[self.buffer_index];

        for (selection_index, selection) in buffer.selections.iter().enumerate() {
            let (cursor_line, cursor_column) = buffer.index_to_coordinates(selection.cursor_index);

            if cursor_line >= buffer.lines_offset() && cursor_column >= buffer.columns_offset() {
                let cursor_line = (cursor_line - buffer.lines_offset()) as u16;
                let cursor_column = (cursor_column - buffer.columns_offset()) as u16;
                let cursor_char = {
                    let char = buffer.contents.char(selection.cursor_index);
                    if char == '\n' {
                        ' '
                    } else {
                        char
                    }
                };

                Terminal::queue(cursor::MoveTo(cursor_column, cursor_line));
                Terminal::queue(style::PrintStyledContent(
                    style::style(cursor_char).on(style::Color::Yellow),
                ));
            }
        }
    }

    fn render(&self) {
        Terminal::queue(terminal::Clear(terminal::ClearType::All));

        self.render_tildes();
        self.render_buffer();
        self.render_selections();

        Terminal::flush();
    }
}
