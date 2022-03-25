use crate::{
    buffer::{self, Buffer},
    command::CommandLine,
    operand::Operand,
    selection,
};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
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
    RunCommand(String),
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
            ChangeMode(mode) => {
                self.mode = mode;
            }
            RunCommand(command) => match command.as_str() {
                "" => {}
                "quit" | "q" => {
                    self.quit = true;
                }
                _ => {
                    unimplemented!("Unknown command: {}", command);
                }
            },
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
    pub fn new() -> Self {
        Editor {
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
                        KeyCode::Char(':') => vec![ChangeMode(Mode::Command)],
                        KeyCode::Char('i') => vec![
                            ChangeMode(Mode::Insert),
                            Buffer(AllSelections(FlipBackwards)),
                        ],
                        // Edit
                        KeyCode::Char('d') => vec![Buffer(Delete)],
                        _ => vec![NoOp],
                    }
                } else {
                    vec![NoOp]
                }
            }
            Event::Resize(_, _) => vec![NoOp],
            Event::Mouse(_) => vec![NoOp],
        };

        // Must always perform an operation, so the count can be reset properly. If no work needs
        // to be done, use `vec![NoOp]`.
        assert!(!operations.is_empty());

        operations
    }

    fn handle_event_insert(&self, event: Event) -> Vec<Operation> {
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
            Event::Resize(_, _) => Vec::new(),
            Event::Mouse(_) => Vec::new(),
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

        let count = if self.count > 0 {
            format!(" {}", self.count)
        } else {
            "".to_string()
        };

        Block::default()
            .title(format!("{}{}", mode, count))
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
