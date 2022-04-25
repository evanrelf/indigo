use crate::buffer::Buffer;
use crossterm::event::{KeyCode, KeyModifiers, MouseEventKind};
use std::{fmt::Display, path::Path};

pub enum Mode {
    Normal,
    Goto,
    Select,
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
                Self::Select => "select",
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
    select_regex: String,
    count: usize,
    buffers: Vec<Buffer>,
    buffer_index: usize,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self {
            quit: false,
            mode: Mode::Normal,
            command: String::new(),
            select_regex: String::new(),
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

    #[must_use]
    pub fn quit(&self) -> bool {
        self.quit
    }

    #[must_use]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    #[must_use]
    pub fn command(&self) -> &String {
        &self.command
    }

    #[must_use]
    pub fn select_regex(&self) -> &String {
        &self.select_regex
    }

    #[must_use]
    pub fn count(&self) -> usize {
        self.count
    }

    #[must_use]
    pub fn buffers(&self) -> &Vec<Buffer> {
        &self.buffers
    }

    #[must_use]
    pub fn buffer_index(&self) -> usize {
        self.buffer_index
    }

    pub fn handle_event(&mut self, event: Event) {
        match self.mode {
            Mode::Normal => self.handle_event_normal(event),
            Mode::Goto => self.handle_event_goto(event),
            Mode::Select => self.handle_event_select(event),
            Mode::Command => self.handle_event_command(event),
            Mode::Insert => self.handle_event_insert(event),
        }
    }

    fn handle_event_normal(&mut self, event: Event) {
        use Event::*;

        let old_count = self.count;

        let count = if self.count == 0 { 1 } else { self.count };

        let buffer = &mut self.buffers[self.buffer_index];

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::SHIFT => {
                match key_event.code {
                    // Move
                    KeyCode::Char('H') => buffer.extend_left(count),
                    KeyCode::Char('J') => buffer.extend_down(count),
                    KeyCode::Char('K') => buffer.extend_up(count),
                    KeyCode::Char('L') => buffer.extend_right(count),
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => self.quit = true,
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::ALT =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char(';') => buffer.selection.in_all_ranges(|range| {
                        range.flip_mut();
                    }),
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
                    KeyCode::Up => buffer.scroll_up(count),
                    KeyCode::Down => buffer.scroll_down(count),
                    KeyCode::Left => buffer.scroll_left(count),
                    KeyCode::Right => buffer.scroll_right(count),
                    // Move
                    KeyCode::Char('h') => buffer.move_left(count),
                    KeyCode::Char('j') => buffer.move_down(count),
                    KeyCode::Char('k') => buffer.move_up(count),
                    KeyCode::Char('l') => buffer.move_right(count),
                    // Mode
                    KeyCode::Char('g') => self.mode = Mode::Goto,
                    KeyCode::Char(':') => self.mode = Mode::Command,
                    KeyCode::Char('i') => {
                        self.mode = Mode::Insert;
                        buffer.selection.in_all_ranges(|range| {
                            range.flip_backwards_mut();
                        });
                    }
                    // Selection
                    KeyCode::Char('s') => self.mode = Mode::Select,
                    KeyCode::Char(';') => {
                        buffer.selection.in_all_ranges(|range| {
                            range.reduce_mut();
                        });
                    }
                    KeyCode::Char(',') => {
                        let primary = buffer.selection.primary_range_index;
                        buffer.selection.filter_ranges(|i, _| i == primary);
                    }
                    // Edit
                    KeyCode::Char('d') => buffer.delete(),
                    _ => {}
                }
            }
            Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => buffer.scroll_up(3),
                MouseEventKind::ScrollDown => buffer.scroll_down(3),
                _ => {}
            },
            Key(_) => {}
        };

        let new_count = self.count;

        if old_count == new_count {
            self.count = 0;
        }
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
                    KeyCode::Char('K') => self.buffers[self.buffer_index].extend_buffer_top(),
                    KeyCode::Char('J') => self.buffers[self.buffer_index].extend_buffer_bottom(),
                    KeyCode::Char('E') => self.buffers[self.buffer_index].extend_buffer_end(),
                    KeyCode::Char('H') => self.buffers[self.buffer_index].extend_line_begin(),
                    KeyCode::Char('I') => {
                        self.buffers[self.buffer_index].extend_line_first_non_blank()
                    }
                    KeyCode::Char('L') => self.buffers[self.buffer_index].extend_line_end(),
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => self.mode = Mode::Normal,
                KeyCode::Char('k') => self.buffers[self.buffer_index].move_buffer_top(),
                KeyCode::Char('j') => self.buffers[self.buffer_index].move_buffer_bottom(),
                KeyCode::Char('e') => self.buffers[self.buffer_index].move_buffer_end(),
                KeyCode::Char('h') => self.buffers[self.buffer_index].move_line_begin(),
                KeyCode::Char('i') => self.buffers[self.buffer_index].move_line_first_non_blank(),
                KeyCode::Char('l') => self.buffers[self.buffer_index].move_line_end(),
                _ => {}
            },
            Mouse(_) => {}
            Key(_) => {}
        };

        self.mode = Mode::Normal;
    }

    pub fn handle_event_select(&mut self, event: Event) {
        use Event::*;

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL =>
            {
                #[allow(clippy::single_match)]
                match key_event.code {
                    KeyCode::Char('c') => {
                        self.select_regex.clear();
                        self.mode = Mode::Normal;
                    }
                    _ => {}
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => {
                    self.select_regex.clear();
                    self.mode = Mode::Normal;
                }
                KeyCode::Char(c) => {
                    self.select_regex.push(c);
                }
                KeyCode::Backspace => {
                    if self.select_regex.pop().is_none() {
                        self.select_regex.clear();
                        self.mode = Mode::Normal;
                    }
                }
                KeyCode::Enter => {
                    // TODO
                    let buffer = &mut self.buffers[self.buffer_index];
                    let rope = (*buffer.rope()).clone(); // TODO
                    let regex = regex::Regex::new(&self.select_regex).unwrap();
                    buffer.selection.select(&rope, &regex);
                    self.select_regex.clear();
                    self.mode = Mode::Normal;
                }
                _ => {}
            },
            Key(_) | Mouse(_) => {}
        }
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