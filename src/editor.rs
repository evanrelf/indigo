use crate::buffer::Buffer;
use crossterm::event::{KeyCode, KeyModifiers, MouseEventKind};
use std::{fmt::Display, path::Path};

pub enum Mode {
    Normal,
    Goto,
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
                Self::Goto => "goto",
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
        match self.mode {
            Mode::Normal => self.handle_event_normal(event),
            Mode::Goto => self.handle_event_goto(event),
            Mode::Command => self.handle_event_command(event),
            Mode::Insert => self.handle_event_insert(event),
        }
    }

    fn handle_event_normal(&mut self, event: Event) {
        use Event::*;

        let count = if self.count == 0 { 1 } else { self.count };

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => self.quit = true,
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Move
                    KeyCode::Char('H') => self.buffers[self.buffer_index].extend_left(count),
                    KeyCode::Char('J') => self.buffers[self.buffer_index].extend_down(count),
                    KeyCode::Char('K') => self.buffers[self.buffer_index].extend_up(count),
                    KeyCode::Char('L') => self.buffers[self.buffer_index].extend_right(count),
                    _ => {}
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
                        self.count = new_count;
                    }
                    // Scroll
                    KeyCode::Up => self.buffers[self.buffer_index].scroll_up(count),
                    KeyCode::Down => self.buffers[self.buffer_index].scroll_down(count),
                    KeyCode::Left => self.buffers[self.buffer_index].scroll_left(count),
                    KeyCode::Right => self.buffers[self.buffer_index].scroll_right(count),
                    // Move
                    KeyCode::Char('h') => self.buffers[self.buffer_index].move_left(count),
                    KeyCode::Char('j') => self.buffers[self.buffer_index].move_down(count),
                    KeyCode::Char('k') => self.buffers[self.buffer_index].move_up(count),
                    KeyCode::Char('l') => self.buffers[self.buffer_index].move_right(count),
                    // Mode
                    KeyCode::Char('g') => self.mode = Mode::Goto,
                    KeyCode::Char(':') => self.mode = Mode::Command,
                    KeyCode::Char('i') => {
                        self.mode = Mode::Insert;
                        self.buffers[self.buffer_index]
                            .selection
                            .in_all_ranges(|range| { range.flip_backwards(); });
                    }
                    // Edit
                    KeyCode::Char('d') => self.buffers[self.buffer_index].delete(),
                    _ => {}
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => self.buffers[self.buffer_index].scroll_up(3),
                MouseEventKind::ScrollDown => self.buffers[self.buffer_index].scroll_down(3),
                _ => {}
            },
            Key(_) => {}
        };

        // TODO: Restore count resetting
        // Must always perform an operation, so the count can be reset properly. If no work
        // needs to be done, we use `vec![NoOp]`.
    }

    fn handle_event_goto(&mut self, event: Event) {
        use Event::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => self.quit = true,
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Move
                    KeyCode::Char('J') => self.buffers[self.buffer_index].extend_bottom(),
                    KeyCode::Char('K') => self.buffers[self.buffer_index].extend_top(),
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => self.mode = Mode::Normal,
                KeyCode::Char('j') => self.buffers[self.buffer_index].move_bottom(),
                KeyCode::Char('k') => self.buffers[self.buffer_index].move_top(),
                _ => {}
            },
            Mouse(_) => {}
            Key(_) => {}
        };

        self.mode = Mode::Normal;
    }

    pub fn handle_event_command(&mut self, event: Event) {
        use Event::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => {
                        self.command.clear();
                        self.mode = Mode::Normal;
                    }
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => {
                    self.command.clear();
                    self.mode = Mode::Normal;
                }
                KeyCode::Char(c) => {
                    self.command.push(c);
                }
                KeyCode::Backspace => {
                    if self.command.pop().is_none() {
                        self.command.clear();
                        self.mode = Mode::Normal;
                    }
                }
                KeyCode::Enter => {
                    self.run_command();
                    self.command.clear();
                }
                _ => {}
            },
            Key(_) | Mouse(_) => {}
        }
    }

    fn handle_event_insert(&mut self, event: Event) {
        use Event::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => self.quit = true,
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Edit
                    KeyCode::Char(c) => self.buffers[self.buffer_index].insert(c),
                    KeyCode::Enter => self.buffers[self.buffer_index].insert('\n'),
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => {
                match key_event.code {
                    // Scroll
                    KeyCode::Up => self.buffers[self.buffer_index].scroll_up(1),
                    KeyCode::Down => self.buffers[self.buffer_index].scroll_down(1),
                    KeyCode::Left => self.buffers[self.buffer_index].scroll_left(1),
                    KeyCode::Right => self.buffers[self.buffer_index].scroll_right(1),
                    // Mode
                    KeyCode::Esc => self.mode = Mode::Normal,
                    // Edit
                    KeyCode::Char(c) => self.buffers[self.buffer_index].insert(c),
                    KeyCode::Enter => self.buffers[self.buffer_index].insert('\n'),
                    KeyCode::Backspace => self.buffers[self.buffer_index].backspace(),
                    _ => {}
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => self.buffers[self.buffer_index].scroll_up(3),
                MouseEventKind::ScrollDown => self.buffers[self.buffer_index].scroll_down(3),
                _ => {}
            },
            Key(_) => {}
        }
    }

    fn run_command(&mut self) {
        match self.command.split_ascii_whitespace().collect::<Vec<_>>()[..] {
            ["buffer-next" | "bn"] => {
                self.mode = Mode::Normal;
                self.buffer_index += 1;
            }
            ["buffer-prev" | "bp"] => {
                self.mode = Mode::Normal;
                self.buffer_index -= 1;
            }
            ["quit" | "q"] => {
                self.quit = true;
            }
            [] => self.mode = Mode::Normal,
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
