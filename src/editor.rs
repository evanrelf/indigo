use crate::{buffer::Buffer, operand::Operand};
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
    command: String,
    count: usize,
    buffers: Vec<Buffer>,
    buffer_index: usize,
}

impl Editor {
    pub fn new() -> Self {
        Self {
            quit: false,
            mode: Mode::Normal,
            command: String::new(),
            count: 0,
            buffers: vec![Buffer::new()],
            buffer_index: 0,
        }
    }

    pub fn open<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        self.buffers.push(Buffer::open(path));
        self.buffer_index += 1;
    }

    pub fn quit(&self) -> bool {
        self.quit
    }

    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    pub fn command(&self) -> &String {
        &self.command
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
            Mode::Command => self.handle_event_command(event),
            Mode::Insert => self.handle_event_insert(event),
        };

        for operation in operations {
            self.apply(operation);
        }
    }

    fn handle_event_normal(&mut self, event: Event) -> Vec<Operation> {
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
                    KeyCode::Char('H') => vec![InBuffer(Box::new(move |buffer| buffer.move_left(count)))],
                    KeyCode::Char('J') => vec![InBuffer(Box::new(move |buffer| buffer.move_down(count)))],
                    KeyCode::Char('K') => vec![InBuffer(Box::new(move |buffer| buffer.move_up(count)))],
                    KeyCode::Char('L') => vec![InBuffer(Box::new(move |buffer| buffer.move_right(count)))],
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
                    KeyCode::Up => vec![InBuffer(Box::new(move |buffer| buffer.scroll_up(count)))],
                    KeyCode::Down => vec![InBuffer(Box::new(move |buffer| buffer.scroll_down(count)))],
                    KeyCode::Left => vec![InBuffer(Box::new(move |buffer| buffer.scroll_left(count)))],
                    KeyCode::Right => vec![InBuffer(Box::new(move |buffer| buffer.scroll_right(count)))],
                    // Move
                    KeyCode::Char('h') => vec![
                        InBuffer(Box::new(move |buffer| buffer.move_left(count))),
                        InBuffer(Box::new(move |buffer| buffer.selection.in_all_ranges(|range| range.reduce()))),
                    ],
                    KeyCode::Char('j') => vec![
                        InBuffer(Box::new(move |buffer| buffer.move_down(count))),
                        InBuffer(Box::new(move |buffer| buffer.selection.in_all_ranges(|range| range.reduce()))),
                    ],
                    KeyCode::Char('k') => vec![
                        InBuffer(Box::new(move |buffer| buffer.move_up(count))),
                        InBuffer(Box::new(move |buffer| buffer.selection.in_all_ranges(|range| range.reduce()))),
                    ],
                    KeyCode::Char('l') => vec![
                        InBuffer(Box::new(move |buffer| buffer.move_right(count))),
                        InBuffer(Box::new(move |buffer| buffer.selection.in_all_ranges(|range| range.reduce()))),
                    ],
                    // Mode
                    KeyCode::Char(':') => vec![ChangeMode(Mode::Command)],
                    KeyCode::Char('i') => vec![
                        ChangeMode(Mode::Insert),
                        InBuffer(Box::new(move |buffer| {
                            buffer
                                .selection
                                .in_all_ranges(|range| range.flip_backwards())
                        })),
                    ],
                    // Edit
                    KeyCode::Char('d') => vec![InBuffer(Box::new(move |buffer| buffer.delete()))],
                    _ => Vec::new(),
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Box::new(move |buffer| buffer.scroll_up(3)))],
                MouseEventKind::ScrollDown => vec![InBuffer(Box::new(move |buffer| buffer.scroll_down(3)))],
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

    pub fn handle_event_command(&mut self, event: Event) -> Vec<Operation> {
        use Event::*;
        use Mode::*;
        use Operation::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => {
                        self.command.clear();
                        vec![ChangeMode(Normal)]
                    }
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => {
                    self.command.clear();
                    vec![ChangeMode(Normal)]
                }
                KeyCode::Char(c) => {
                    self.command.push(c);
                    Vec::new()
                }
                KeyCode::Backspace => match self.command.pop() {
                    Some(_) => Vec::new(),
                    None => {
                        self.command.clear();
                        vec![ChangeMode(Normal)]
                    }
                },
                KeyCode::Enter => {
                    let operations = self.run_command();
                    self.command.clear();
                    operations
                }
                _ => Vec::new(),
            },
            Key(_) | Mouse(_) => Vec::new(),
        }
    }

    fn handle_event_insert(&self, event: Event) -> Vec<Operation> {
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
                    KeyCode::Char(c) => vec![InBuffer(Box::new(move |buffer| buffer.insert(c)))],
                    KeyCode::Enter => vec![InBuffer(Box::new(move |buffer| buffer.insert('\n')))],
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
                match key_event.code {
                    // Scroll
                    KeyCode::Up => vec![InBuffer(Box::new(move |buffer| buffer.scroll_up(1)))],
                    KeyCode::Down => vec![InBuffer(Box::new(move |buffer| buffer.scroll_down(1)))],
                    KeyCode::Left => vec![InBuffer(Box::new(move |buffer| buffer.scroll_left(1)))],
                    KeyCode::Right => vec![InBuffer(Box::new(move |buffer| buffer.scroll_right(1)))],
                    // Mode
                    KeyCode::Esc => vec![ChangeMode(Mode::Normal)],
                    // Edit
                    KeyCode::Char(c) => vec![InBuffer(Box::new(move |buffer| buffer.insert(c)))],
                    KeyCode::Enter => vec![InBuffer(Box::new(move |buffer| buffer.insert('\n')))],
                    KeyCode::Backspace => vec![InBuffer(Box::new(move |buffer| buffer.backspace()))],
                    _ => vec![],
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => vec![InBuffer(Box::new(move |buffer| buffer.scroll_up(3)))],
                MouseEventKind::ScrollDown => vec![InBuffer(Box::new(move |buffer| buffer.scroll_down(3)))],
                _ => vec![],
            },
            Key(_) => Vec::new(),
        }
    }

    fn run_command(&self) -> Vec<Operation> {
        use Mode::*;
        use Operation::*;

        match self.command.split_ascii_whitespace().collect::<Vec<_>>()[..] {
            ["buffer-next" | "bn"] => {
                vec![ChangeMode(Normal), NextBuffer]
            }
            ["buffer-prev" | "bp"] => {
                vec![ChangeMode(Normal), PreviousBuffer]
            }
            ["quit" | "q"] => {
                vec![Quit]
            }
            [] => vec![ChangeMode(Normal)],
            _ => {
                unimplemented!("Unknown command: {}", self.command);
            }
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
    NextBuffer,
    PreviousBuffer,
    InBuffer(Box<dyn Fn(&mut Buffer)>),
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
            NextBuffer => {
                self.buffer_index += 1;
            }
            PreviousBuffer => {
                self.buffer_index -= 1;
            }
            NoOp => {}
            InBuffer(_) => {
                // f(&mut self.buffers[self.buffer_index]);
            }
        }

        let new_count = self.count;

        if old_count == new_count {
            self.count = 0;
        }
    }
}
