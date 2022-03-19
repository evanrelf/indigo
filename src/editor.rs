use crate::{
    buffer::{self, Buffer},
    terminal::Terminal,
};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    style::{self, Stylize},
    terminal,
};
use std::path::Path;

pub(crate) enum Mode {
    Normal,
    Insert,
}

pub(crate) struct Editor {
    quit: bool,
    mode: Mode,
    count: usize,
    buffers: Vec<Buffer>,
    buffer_index: usize,
    viewport_lines: u16,
    viewport_columns: u16,
}

pub(crate) enum Operation {
    Quit,
    Resize { lines: u16, columns: u16 },
    ChangeMode(Mode),
    SetCount(usize),
    Buffer(buffer::Operation),
    NoOp,
}

impl Editor {
    pub(crate) fn new() -> Editor {
        let (viewport_columns, viewport_lines) = Terminal::size();

        Editor {
            quit: false,
            mode: Mode::Normal,
            count: 0,
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
            let viewport_start_line = buffer.viewport_lines_offset();
            let viewport_end_line = viewport_start_line + (self.viewport_lines as usize - 1);
            let buffer_end_line = buffer.rope().len_lines();
            let start = buffer.rope().line_to_char(viewport_start_line);
            if buffer_end_line <= viewport_end_line {
                buffer.rope().slice(start..)
            } else {
                let end = buffer.rope().line_to_char(viewport_end_line);
                buffer.rope().slice(start..end)
            }
        };

        for (i, line) in viewport_slice.lines().enumerate() {
            if line
                .len_chars()
                .saturating_sub(buffer.viewport_columns_offset())
                > 0
            {
                let line = line.slice(buffer.viewport_columns_offset()..);
                Terminal::queue(cursor::MoveTo(0, i as u16));
                Terminal::queue(terminal::Clear(terminal::ClearType::CurrentLine));
                Terminal::queue(style::Print(String::from(line).trim_end()));
            }
        }
    }

    fn render_selections(&self) {
        let buffer = &self.buffers[self.buffer_index];

        for selection_mutex in buffer.selections() {
            let selection = selection_mutex.lock().unwrap();

            let anchor_visible = selection.anchor.line >= buffer.viewport_lines_offset()
                && selection.anchor.column >= buffer.viewport_columns_offset();

            if anchor_visible && !selection.is_reduced() {
                let anchor_line = (selection.anchor.line - buffer.viewport_lines_offset()) as u16;
                let anchor_column =
                    (selection.anchor.column - buffer.viewport_columns_offset()) as u16;
                let anchor_char = {
                    let anchor_index = buffer.cursor_to_index(&selection.anchor).unwrap();
                    let char = buffer.rope().char(anchor_index);
                    if char == '\n' {
                        ' '
                    } else {
                        char
                    }
                };

                Terminal::queue(cursor::MoveTo(anchor_column, anchor_line));
                Terminal::queue(style::PrintStyledContent(anchor_char.on_cyan()));
            }

            let cursor_visible = selection.cursor.line >= buffer.viewport_lines_offset()
                && selection.cursor.column >= buffer.viewport_columns_offset();

            if cursor_visible {
                let cursor_line = (selection.cursor.line - buffer.viewport_lines_offset()) as u16;
                let cursor_column =
                    (selection.cursor.column - buffer.viewport_columns_offset()) as u16;
                let cursor_char = {
                    let cursor_index = buffer.cursor_to_index(&selection.cursor).unwrap();
                    let char = buffer.rope().char(cursor_index);
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
        Terminal::queue(cursor::MoveTo(0, self.viewport_lines));

        let mode = match self.mode {
            Mode::Normal => "normal",
            Mode::Insert => "insert",
        };
        Terminal::queue(style::PrintStyledContent(mode.white().on_black()));

        if self.count > 0 {
            Terminal::queue(style::Print(format!(" {}", self.count)));
        }
    }

    fn handle_event(&mut self) {
        let event = event::read().unwrap();

        let operations = match self.mode {
            Mode::Normal => self.event_to_operations_normal(event),
            Mode::Insert => self.event_to_operations_insert(event),
        };

        for operation in operations {
            self.apply_operation(operation);
        }
    }

    fn event_to_operations_normal(&self, event: event::Event) -> Vec<Operation> {
        use buffer::Operation::*;
        use Operation::*;

        let count = if self.count == 0 { 1 } else { self.count };

        let operations = match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    match code {
                        KeyCode::Char('c') => vec![Quit],
                        _ => vec![NoOp],
                    }
                } else if modifiers == KeyModifiers::SHIFT {
                    match code {
                        // Move
                        KeyCode::Char('H') => vec![Buffer(MoveLeft(count))],
                        KeyCode::Char('J') => vec![Buffer(MoveDown(count))],
                        KeyCode::Char('K') => vec![Buffer(MoveUp(count))],
                        KeyCode::Char('L') => vec![Buffer(MoveRight(count))],
                        _ => vec![NoOp],
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Count
                        KeyCode::Char(c) if ('0'..='9').contains(&c) => {
                            let n = c.to_string().parse().unwrap();
                            let new_count = if self.count == 0 {
                                n
                            } else {
                                (self.count * 10) + n
                            };
                            vec![SetCount(new_count)]
                        }
                        // Scroll
                        KeyCode::Up => vec![Buffer(ScrollUp(count))],
                        KeyCode::Down => vec![Buffer(ScrollDown(count))],
                        KeyCode::Left => vec![Buffer(ScrollLeft(count))],
                        KeyCode::Right => vec![Buffer(ScrollRight(count))],
                        // Move
                        KeyCode::Char('h') => vec![Buffer(MoveLeft(count)), Buffer(Reduce)],
                        KeyCode::Char('j') => vec![Buffer(MoveDown(count)), Buffer(Reduce)],
                        KeyCode::Char('k') => vec![Buffer(MoveUp(count)), Buffer(Reduce)],
                        KeyCode::Char('l') => vec![Buffer(MoveRight(count)), Buffer(Reduce)],
                        // Mode
                        KeyCode::Char('i') => vec![ChangeMode(Mode::Insert), Buffer(FlipBackwards)],
                        // Edit
                        KeyCode::Char('d') => vec![Buffer(Delete)],
                        _ => vec![NoOp],
                    }
                } else {
                    vec![NoOp]
                }
            }
            Event::Resize(columns, lines) => vec![Resize { lines, columns }],
            _ => vec![NoOp],
        };

        // Must always perform an operation, so the count can be reset properly. If no work needs
        // to be done, use `vec![NoOp]`.
        assert!(!operations.is_empty());

        operations
    }

    fn event_to_operations_insert(&self, event: event::Event) -> Vec<Operation> {
        use buffer::Operation::*;
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
                        KeyCode::Char(c) => vec![Buffer(Insert(c))],
                        KeyCode::Enter => vec![Buffer(Insert('\n'))],
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        // Scroll
                        KeyCode::Up => vec![Buffer(ScrollUp(1))],
                        KeyCode::Down => vec![Buffer(ScrollDown(1))],
                        KeyCode::Left => vec![Buffer(ScrollLeft(1))],
                        KeyCode::Right => vec![Buffer(ScrollRight(1))],
                        // Mode
                        KeyCode::Esc => vec![ChangeMode(Mode::Normal)],
                        // Edit
                        KeyCode::Char(c) => vec![Buffer(Insert(c))],
                        KeyCode::Enter => vec![Buffer(Insert('\n'))],
                        KeyCode::Backspace => vec![Buffer(Backspace)],
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

        let old_count = self.count;

        match operation {
            Quit => {
                self.quit = true;
            }
            Resize { lines, columns } => {
                self.viewport_lines = lines;
                self.viewport_columns = columns;
            }
            ChangeMode(mode) => {
                self.mode = mode;
            }
            SetCount(new_count) => {
                self.count = new_count;
            }
            Buffer(operation) => {
                self.buffers[self.buffer_index].apply_operation(operation);
            }
            NoOp => {}
        }

        let new_count = self.count;

        if old_count == new_count {
            self.count = 0;
        }
    }
}
