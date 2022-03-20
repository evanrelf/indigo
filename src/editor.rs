use crate::{
    buffer::{self, Buffer},
    operand::Operand,
    selection,
    terminal::Terminal,
};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use std::path::Path;
use tui::widgets::Widget;

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

impl Operand for Editor {
    type Operation = Operation;

    fn apply(&mut self, operation: Self::Operation) {
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
                self.buffers[self.buffer_index].apply(operation);
            }
            NoOp => {}
        }

        let new_count = self.count;

        if old_count == new_count {
            self.count = 0;
        }
    }
}

impl Editor {
    pub(crate) fn new() -> Self {
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

    pub(crate) fn run<B>(&mut self, terminal: &mut tui::Terminal<B>)
    where
        B: tui::backend::Backend,
    {
        while !self.quit {
            terminal
                .draw(|frame| frame.render_widget(&*self, frame.size()))
                .unwrap();
            self.handle_event();
        }
    }

    fn handle_event(&mut self) {
        let event = event::read().unwrap();

        let operations = match self.mode {
            Mode::Normal => self.event_to_operations_normal(event),
            Mode::Insert => self.event_to_operations_insert(event),
        };

        for operation in operations {
            self.apply(operation);
        }
    }

    fn event_to_operations_normal(&self, event: event::Event) -> Vec<Operation> {
        use buffer::Operation::*;
        use selection::Operation::*;
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
                        KeyCode::Char('h') => {
                            vec![Buffer(MoveLeft(count)), Buffer(AllSelections(Reduce))]
                        }
                        KeyCode::Char('j') => {
                            vec![Buffer(MoveDown(count)), Buffer(AllSelections(Reduce))]
                        }
                        KeyCode::Char('k') => {
                            vec![Buffer(MoveUp(count)), Buffer(AllSelections(Reduce))]
                        }
                        KeyCode::Char('l') => {
                            vec![Buffer(MoveRight(count)), Buffer(AllSelections(Reduce))]
                        }
                        // Mode
                        KeyCode::Char('i') => {
                            vec![
                                ChangeMode(Mode::Insert),
                                Buffer(AllSelections(FlipBackwards)),
                            ]
                        }
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
}

impl Widget for &Editor {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::{
            buffer::Buffer,
            layout::{Constraint, Direction, Layout, Rect},
            style::{Color, Style},
            widgets::Block,
        };

        let render_tildes = |area: Rect, buffer: &mut Buffer| {
            for y in area.top()..=area.bottom() {
                buffer.get_mut(0, y).set_char('~');
            }
        };

        let render_status = |area: Rect, buffer: &mut Buffer| {
            let mode = match self.mode {
                Mode::Normal => "normal",
                Mode::Insert => "insert",
            };

            let count = if self.count > 0 {
                format!(" {}", self.count)
            } else {
                "".to_string()
            };

            Block::default()
                .title(format!("{}{}", mode, count))
                .style(Style::default().bg(Color::White))
                .render(area, buffer)
        };

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(1), Constraint::Max(1)].as_ref())
            .split(area);

        render_tildes(chunks[0], buffer);
        render_status(chunks[1], buffer);
        self.buffers[self.buffer_index].render(chunks[0], buffer);
    }
}
