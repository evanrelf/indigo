use crate::{
    buffer::{self, Buffer},
    command::CommandLine,
    direction,
    operand::Operand,
    range, selection,
};
use crossterm::event::{KeyCode, KeyModifiers, MouseEventKind};
use std::{fmt::Display, path::Path};

pub enum Mode {
    Normal,
    Command,
    Insert,
}

impl Display for Mode {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            formatter,
            "{}",
            match self {
                Self::Normal => "normal",
                Self::Command => "command",
                Self::Insert => "insert",
            }
        )
    }
}

pub struct Editor {
    quit: bool,
    mode: Mode,
    command_line: CommandLine,
    count: usize,
    buffers: Vec<Buffer>,
    buffer_index: usize,
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

    pub fn quit(&self) -> bool {
        self.quit
    }

    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    pub fn command_line(&self) -> &CommandLine {
        &self.command_line
    }

    pub fn count(&self) -> usize {
        self.count
    }

    pub fn buffers(&self) -> &Vec<Buffer> {
        &self.buffers
    }

    pub fn buffer_index(&self) -> usize {
        self.buffer_index
    }

    pub fn handle_event(&mut self, event: Event) {
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
        use Event::*;
        use Operation::*;

        let count = if self.count == 0 { 1 } else { self.count };

        let operations = match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => vec![Quit],
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Move
                    KeyCode::Char('H') => vec![InBuffer(Move(Left, count))],
                    KeyCode::Char('J') => vec![InBuffer(Move(Down, count))],
                    KeyCode::Char('K') => vec![InBuffer(Move(Up, count))],
                    KeyCode::Char('L') => vec![InBuffer(Move(Right, count))],
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
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
                    _ => Vec::new(),
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Scroll(Up, 3))],
                MouseEventKind::ScrollDown => vec![InBuffer(Scroll(Down, 3))],
                _ => Vec::new(),
            },
            Key(_) => Vec::new(),
        };

        if operations.is_empty() {
            // Must always perform an operation, so the count can be reset properly. If no work
            // needs to be done, we use `vec![NoOp]`.
            vec![NoOp]
        } else {
            operations
        }
    }

    fn handle_event_insert(&self, event: Event) -> Vec<Operation> {
        use buffer::Operation::*;
        use direction::Direction::*;
        use Event::*;
        use Operation::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => vec![Quit],
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Edit
                    KeyCode::Char(c) => vec![InBuffer(Insert(c))],
                    KeyCode::Enter => vec![InBuffer(Insert('\n'))],
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
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
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Scroll(Up, 3))],
                MouseEventKind::ScrollDown => vec![InBuffer(Scroll(Down, 3))],
                _ => vec![],
            },
            Key(_) => Vec::new(),
        }
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Event {
    Key(crossterm::event::KeyEvent),
    Mouse(crossterm::event::MouseEvent),
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
