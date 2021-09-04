use crate::buffer::{Buffer, Mode};
use crate::terminal::Terminal;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::Stylize;
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
        self.render_selections();
        self.render_status();

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
            let viewport_end_line = viewport_start_line + (self.viewport_lines as usize - 1);
            let buffer_end_line = buffer.rope.len_lines();
            let start = buffer.rope.line_to_char(viewport_start_line);
            if buffer_end_line <= viewport_end_line {
                buffer.rope.slice(start..)
            } else {
                let end = buffer.rope.line_to_char(viewport_end_line);
                buffer.rope.slice(start..end)
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

    fn render_selections(&self) {
        let buffer = &self.buffers[self.buffer_index];

        for selection_mutex in &buffer.selections {
            let selection = selection_mutex.lock().unwrap();

            let anchor_visible = selection.anchor.line >= buffer.viewport_lines_offset
                && selection.anchor.column >= buffer.viewport_columns_offset;

            if anchor_visible && !selection.is_reduced() {
                let anchor_line = (selection.anchor.line - buffer.viewport_lines_offset) as u16;
                let anchor_column =
                    (selection.anchor.column - buffer.viewport_columns_offset) as u16;
                let anchor_char = {
                    let anchor_index = selection.anchor.to_index(&buffer.rope).unwrap();
                    let char = buffer.rope.char(anchor_index);
                    if char == '\n' {
                        ' '
                    } else {
                        char
                    }
                };

                Terminal::queue(cursor::MoveTo(anchor_column, anchor_line));
                Terminal::queue(style::PrintStyledContent(anchor_char.on_cyan()));
            }

            let cursor_visible = selection.cursor.line >= buffer.viewport_lines_offset
                && selection.cursor.column >= buffer.viewport_columns_offset;

            if cursor_visible {
                let cursor_line = (selection.cursor.line - buffer.viewport_lines_offset) as u16;
                let cursor_column =
                    (selection.cursor.column - buffer.viewport_columns_offset) as u16;
                let cursor_char = {
                    let cursor_index = selection.cursor.to_index(&buffer.rope).unwrap();
                    let char = buffer.rope.char(cursor_index);
                    if char == '\n' {
                        ' '
                    } else {
                        char
                    }
                };

                Terminal::queue(cursor::MoveTo(cursor_column, cursor_line));
                Terminal::queue(style::PrintStyledContent(cursor_char.on_yellow()));
            }
        }
    }

    fn render_status(&self) {
        let mode = match self.buffers[self.buffer_index].mode {
            Mode::Normal => "normal",
            Mode::Insert => "insert",
        };

        Terminal::queue(cursor::MoveTo(0, self.viewport_lines));
        Terminal::queue(style::PrintStyledContent(mode.white().on_black()));
    }

    fn handle_event(&mut self) {
        let buffer = &mut self.buffers[self.buffer_index];

        let event = event::read().unwrap();

        match buffer.mode {
            Mode::Normal => self.handle_event_normal(event),
            Mode::Insert => self.handle_event_insert(event),
        }
    }

    fn handle_event_normal(&mut self, event: event::Event) {
        let buffer = &mut self.buffers[self.buffer_index];

        match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    #[allow(clippy::single_match)]
                    match code {
                        KeyCode::Char('c') => self.quit = true,
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        // Move
                        KeyCode::Char('H') => {
                            buffer.move_left(1);
                        }
                        KeyCode::Char('J') => {
                            buffer.move_down(1);
                        }
                        KeyCode::Char('K') => {
                            buffer.move_up(1);
                        }
                        KeyCode::Char('L') => {
                            buffer.move_right(1);
                        }
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Scroll
                        KeyCode::Up => {
                            buffer.scroll_up(1);
                        }
                        KeyCode::Down => {
                            buffer.scroll_down(1);
                        }
                        KeyCode::Left => {
                            buffer.scroll_left(1);
                        }
                        KeyCode::Right => {
                            buffer.scroll_right(1);
                        }
                        // Move
                        KeyCode::Char('h') => {
                            buffer.move_left(1).reduce();
                        }
                        KeyCode::Char('j') => {
                            buffer.move_down(1).reduce();
                        }
                        KeyCode::Char('k') => {
                            buffer.move_up(1).reduce();
                        }
                        KeyCode::Char('l') => {
                            buffer.move_right(1).reduce();
                        }
                        // Mode
                        KeyCode::Char('i') => {
                            buffer.mode = Mode::Insert;
                            for selection in &buffer.selections {
                                let mut selection = selection.lock().unwrap();
                                selection.flip_backwards();
                            }
                        }
                        // Edit
                        KeyCode::Char('d') => {
                            buffer.delete();
                        }
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

    fn handle_event_insert(&mut self, event: event::Event) {
        let buffer = &mut self.buffers[self.buffer_index];

        match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    #[allow(clippy::single_match)]
                    match code {
                        KeyCode::Char('c') => self.quit = true,
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        // Edit
                        KeyCode::Char(c) => {
                            buffer.insert(c);
                        }
                        KeyCode::Enter => {
                            buffer.insert('\n');
                        }
                        _ => (),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Scroll
                        KeyCode::Up => {
                            buffer.scroll_up(1);
                        }
                        KeyCode::Down => {
                            buffer.scroll_down(1);
                        }
                        KeyCode::Left => {
                            buffer.scroll_left(1);
                        }
                        KeyCode::Right => {
                            buffer.scroll_right(1);
                        }
                        // Mode
                        KeyCode::Esc => {
                            buffer.mode = Mode::Normal;
                        }
                        // Edit
                        KeyCode::Char(c) => {
                            buffer.insert(c);
                        }
                        KeyCode::Enter => {
                            buffer.insert('\n');
                        }
                        KeyCode::Backspace => {
                            buffer.backspace();
                        }
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
