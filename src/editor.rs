use crate::{
    buffer::{Buffer, Mode},
    terminal::Terminal,
};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    style::{self, Stylize},
    terminal,
};
use std::path::Path;

pub(crate) struct Editor {
    quit: bool,

    buffers: Vec<Buffer>,
    buffer_index: usize,

    viewport_lines: u16,
    viewport_columns: u16,
}

pub(crate) enum Operation {
    Quit,
    Resize { lines: u16, columns: u16 },
    ChangeMode(Mode),

    ScrollUp(usize),
    ScrollDown(usize),
    ScrollLeft(usize),
    ScrollRight(usize),

    MoveUp(usize),
    MoveDown(usize),
    MoveLeft(usize),
    MoveRight(usize),

    Reduce,
    FlipBackwards,

    Insert(char),
    Delete,
    Backspace,
}

impl Editor {
    pub(crate) fn new() -> Editor {
        let (viewport_columns, viewport_lines) = Terminal::size();

        Editor {
            quit: false,

            buffers: vec![Buffer::new()],
            buffer_index: 0,

            viewport_lines,
            viewport_columns,
        }
    }

    pub(crate) fn load_file<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        self.buffers.push(Buffer::from_file(path));
        self.buffer_index += 1;
    }

    pub(crate) fn run(&mut self) {
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
                    let anchor_index = buffer.cursor_to_index(selection.anchor).unwrap();
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
                    let cursor_index = buffer.cursor_to_index(selection.cursor).unwrap();
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
        let buffer = &self.buffers[self.buffer_index];

        let event = event::read().unwrap();

        let operations = match buffer.mode {
            Mode::Normal => self.event_to_operations_normal(event),
            Mode::Insert => self.event_to_operations_insert(event),
        };

        for operation in operations {
            self.apply_operation(operation);
        }
    }

    fn event_to_operations_normal(&self, event: event::Event) -> Vec<Operation> {
        use Operation::*;

        match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    match code {
                        KeyCode::Char('c') => vec![Quit],
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        // Move
                        KeyCode::Char('H') => vec![MoveLeft(1)],
                        KeyCode::Char('J') => vec![MoveDown(1)],
                        KeyCode::Char('K') => vec![MoveUp(1)],
                        KeyCode::Char('L') => vec![MoveRight(1)],
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Scroll
                        KeyCode::Up => vec![ScrollUp(1)],
                        KeyCode::Down => vec![ScrollDown(1)],
                        KeyCode::Left => vec![ScrollLeft(1)],
                        KeyCode::Right => vec![ScrollRight(1)],
                        // Move
                        KeyCode::Char('h') => vec![MoveLeft(1), Reduce],
                        KeyCode::Char('j') => vec![MoveDown(1), Reduce],
                        KeyCode::Char('k') => vec![MoveUp(1), Reduce],
                        KeyCode::Char('l') => vec![MoveRight(1), Reduce],
                        // Mode
                        KeyCode::Char('i') => vec![ChangeMode(Mode::Insert), FlipBackwards],
                        // Edit
                        KeyCode::Char('d') => vec![Delete],
                        _ => Vec::new(),
                    }
                } else {
                    Vec::new()
                }
            }
            Event::Resize(columns, lines) => vec![Resize { lines, columns }],
            _ => Vec::new(),
        }
    }

    fn event_to_operations_insert(&self, event: event::Event) -> Vec<Operation> {
        use Operation::*;

        match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    match code {
                        KeyCode::Char('c') => vec![Quit],
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        // Edit
                        KeyCode::Char(c) => vec![Insert(c)],
                        KeyCode::Enter => vec![Insert('\n')],
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Scroll
                        KeyCode::Up => vec![ScrollUp(1)],
                        KeyCode::Down => vec![ScrollDown(1)],
                        KeyCode::Left => vec![ScrollLeft(1)],
                        KeyCode::Right => vec![ScrollRight(1)],
                        // Mode
                        KeyCode::Esc => vec![ChangeMode(Mode::Normal)],
                        // Edit
                        KeyCode::Char(c) => vec![Insert(c)],
                        KeyCode::Enter => vec![Insert('\n')],
                        KeyCode::Backspace => vec![Backspace],
                        _ => vec![],
                    }
                } else {
                    Vec::new()
                }
            }
            Event::Resize(columns, lines) => vec![Resize { lines, columns }],
            _ => Vec::new(),
        }
    }

    fn apply_operation(&mut self, operation: Operation) {
        use Operation::*;

        let buffer = &mut self.buffers[self.buffer_index];

        match operation {
            Quit => {
                self.quit = true;
            }
            Resize { lines, columns } => {
                self.viewport_lines = lines;
                self.viewport_columns = columns;
            }
            ChangeMode(mode) => {
                buffer.mode = mode;
            }
            ScrollUp(distance) => {
                buffer.scroll_up(distance);
            }
            ScrollDown(distance) => {
                buffer.scroll_down(distance);
            }
            ScrollLeft(distance) => {
                buffer.scroll_left(distance);
            }
            ScrollRight(distance) => {
                buffer.scroll_right(distance);
            }
            MoveUp(distance) => {
                buffer.move_up(distance);
            }
            MoveDown(distance) => {
                buffer.move_down(distance);
            }
            MoveLeft(distance) => {
                buffer.move_left(distance);
            }
            MoveRight(distance) => {
                buffer.move_right(distance);
            }
            Reduce => {
                buffer.reduce();
            }
            FlipBackwards => {
                for selection in &buffer.selections {
                    selection.lock().unwrap().flip_backwards();
                }
            }
            Insert(c) => {
                buffer.insert(c);
            }
            Delete => {
                buffer.delete();
            }
            Backspace => {
                buffer.backspace();
            }
        }
    }
}
