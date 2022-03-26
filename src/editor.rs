use crate::{
    buffer::{self, Buffer},
    command::CommandLine,
    direction,
    operand::Operand,
    range, selection,
};
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use std::path::Path;
use tui::widgets::Widget;

pub enum Mode {
    Normal,
    Command,
    Insert,
}

pub struct Editor {
    quit: bool,
    mode: Mode,
    command_line: CommandLine,
    count: usize,
    buffers: Vec<Buffer>,
    buffer_index: usize,
}

pub enum Operation {
    Quit,
    ChangeMode(Mode),
    SetCount(usize),
    NoOp,
    InBuffer(buffer::Operation),
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
            ChangeMode(mode) => {
                self.mode = mode;
            }
            SetCount(new_count) => {
                self.count = new_count;
            }
            NoOp => {}
            InBuffer(operation) => {
                self.buffers[self.buffer_index].apply(operation);
            }
        }

        let new_count = self.count;

        if old_count == new_count {
            self.count = 0;
        }
    }
}

impl Editor {
    pub fn new() -> Self {
        Self {
            quit: false,
            mode: Mode::Normal,
            command_line: CommandLine::new(),
            count: 0,
            buffers: vec![Buffer::new()],
            buffer_index: 0,
        }
    }

    pub fn load_file<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        self.buffers.push(Buffer::from_file(path));
        self.buffer_index += 1;
    }

    pub fn run<B>(&mut self, terminal: &mut tui::Terminal<B>)
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
            Mode::Normal => self.handle_event_normal(event),
            Mode::Command => self.command_line.handle_event(event),
            Mode::Insert => self.handle_event_insert(event),
        };

        for operation in operations {
            self.apply(operation);
        }
    }

    fn handle_event_normal(&self, event: Event) -> Vec<Operation> {
        use buffer::Operation::*;
        use direction::Direction::*;
        use range::Operation::*;
        use selection::Operation::*;
        use Operation::*;

        let count = if self.count == 0 { 1 } else { self.count };

        let operations = match event {
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => vec![Quit],
                    _ => vec![NoOp],
                }
            }
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Move
                    KeyCode::Char('H') => vec![InBuffer(Move(Left, count))],
                    KeyCode::Char('J') => vec![InBuffer(Move(Down, count))],
                    KeyCode::Char('K') => vec![InBuffer(Move(Up, count))],
                    KeyCode::Char('L') => vec![InBuffer(Move(Right, count))],
                    _ => vec![NoOp],
                }
            }
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
                match key_event.code {
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
                    KeyCode::Up => vec![InBuffer(Scroll(Up, count))],
                    KeyCode::Down => vec![InBuffer(Scroll(Down, count))],
                    KeyCode::Left => vec![InBuffer(Scroll(Left, count))],
                    KeyCode::Right => vec![InBuffer(Scroll(Right, count))],
                    // Move
                    KeyCode::Char('h') => vec![
                        InBuffer(Move(Left, count)),
                        InBuffer(InSelection(InAllRanges(Reduce))),
                    ],
                    KeyCode::Char('j') => vec![
                        InBuffer(Move(Down, count)),
                        InBuffer(InSelection(InAllRanges(Reduce))),
                    ],
                    KeyCode::Char('k') => vec![
                        InBuffer(Move(Up, count)),
                        InBuffer(InSelection(InAllRanges(Reduce))),
                    ],
                    KeyCode::Char('l') => vec![
                        InBuffer(Move(Right, count)),
                        InBuffer(InSelection(InAllRanges(Reduce))),
                    ],
                    // Mode
                    KeyCode::Char(':') => vec![ChangeMode(Mode::Command)],
                    KeyCode::Char('i') => vec![
                        ChangeMode(Mode::Insert),
                        InBuffer(InSelection(InAllRanges(FlipBackwards))),
                    ],
                    // Edit
                    KeyCode::Char('d') => vec![InBuffer(Delete)],
                    _ => vec![NoOp],
                }
            }
            Event::Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Scroll(Up, 3))],
                MouseEventKind::ScrollDown => vec![InBuffer(Scroll(Down, 3))],
                _ => vec![NoOp],
            },
            Event::Key(_) | Event::Resize(_, _) => vec![NoOp],
        };

        // Must always perform an operation, so the count can be reset properly. If no work needs
        // to be done, use `vec![NoOp]`.
        assert!(!operations.is_empty());

        operations
    }

    fn handle_event_insert(&self, event: Event) -> Vec<Operation> {
        use buffer::Operation::*;
        use direction::Direction::*;
        use Operation::*;

        match event {
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => vec![Quit],
                    _ => Vec::new(),
                }
            }
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Edit
                    KeyCode::Char(c) => vec![InBuffer(Insert(c))],
                    KeyCode::Enter => vec![InBuffer(Insert('\n'))],
                    _ => Vec::new(),
                }
            }
            Event::Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
                match key_event.code {
                    // Scroll
                    KeyCode::Up => vec![InBuffer(Scroll(Up, 1))],
                    KeyCode::Down => vec![InBuffer(Scroll(Down, 1))],
                    KeyCode::Left => vec![InBuffer(Scroll(Left, 1))],
                    KeyCode::Right => vec![InBuffer(Scroll(Right, 1))],
                    // Mode
                    KeyCode::Esc => vec![ChangeMode(Mode::Normal)],
                    // Edit
                    KeyCode::Char(c) => vec![InBuffer(Insert(c))],
                    KeyCode::Enter => vec![InBuffer(Insert('\n'))],
                    KeyCode::Backspace => vec![InBuffer(Backspace)],
                    _ => vec![],
                }
            }
            Event::Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Scroll(Up, 3))],
                MouseEventKind::ScrollDown => vec![InBuffer(Scroll(Down, 3))],
                _ => vec![],
            },
            Event::Key(_) | Event::Resize(_, _) => Vec::new(),
        }
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Widget for &Editor {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::{
            layout::{Constraint, Direction, Layout},
            style::{Color, Style},
            widgets::Block,
        };

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Min(0),
                    Constraint::Length(1),
                    Constraint::Length(1),
                ]
                .as_ref(),
            )
            .split(area);

        // Render tildes
        for y in chunks[0].top()..chunks[0].bottom() {
            buffer.get_mut(area.x, y).set_char('~');
        }

        // Render status
        let mode = match self.mode {
            Mode::Normal => "normal",
            Mode::Command => "command",
            Mode::Insert => "insert",
        };

        let cursor = {
            let selection = self.buffers[self.buffer_index].selection();
            let range = &selection.ranges[selection.primary_range_index];
            range.head.to_string()
        };

        let count = if self.count > 0 {
            format!(" {}", self.count)
        } else {
            "".to_string()
        };

        Block::default()
            .title(format!("{} {}{}", mode, cursor, count))
            .style(Style::default().bg(Color::Rgb(0xEE, 0xEE, 0xEE)))
            .render(chunks[1], buffer);

        // Render command line
        if let Mode::Command = self.mode {
            self.command_line.render(chunks[2], buffer);
        }

        // Render buffer
        self.buffers[self.buffer_index].render(chunks[0], buffer);
    }
}
