use crate::buffer::Buffer;
use crate::selection::{Position, Selection};
use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::style::Colorize;
use crossterm::Result;
use ropey::Rope;
use std::borrow::Cow;

pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub enum Action {
    Quit,
    InsertChar(usize, usize, char),
    DeleteChar(usize, usize),
    MoveCursor(Direction, usize),
    MoveAnchor(Direction, usize),
    ReduceSelection,
    FlipSelection,
}

#[derive(Debug)]
pub enum Mode {
    Insert,
    Normal,
}

pub struct Editor {
    pub quit: bool,
    pub title: String,
    pub mode: Mode,
    pub selection: Selection,
    pub buffer: Buffer,
}

impl Editor {
    pub fn new() -> Self {
        Editor {
            quit: false,
            title: String::from("ind"),
            mode: Mode::Insert,
            selection: Selection::new(),
            buffer: Buffer::new(Rope::new()),
        }
    }

    pub fn run(mut self) {
        Terminal::enter();
        Terminal::hide_cursor();
        Terminal::flush();

        while !self.quit {
            self.render();
            let actions = self.handle_event().unwrap();
            for action in actions {
                self.apply_action(action);
            }
        }

        Terminal::exit();
    }

    pub fn handle_event(&self) -> Result<Vec<Action>> {
        match event::read()? {
            Event::Key(key_event) => self.handle_key_event(key_event),
            Event::Mouse(mouse_event) => self.handle_mouse_event(mouse_event),
            Event::Resize(width, height) => self.handle_resize_event(width, height),
        }
    }

    fn handle_key_event(&self, key_event: KeyEvent) -> Result<Vec<Action>> {
        let KeyEvent { code, modifiers } = key_event;

        Ok(if modifiers == KeyModifiers::NONE {
            match code {
                KeyCode::Char(c) => vec![
                    Action::InsertChar(self.selection.cursor.line, self.selection.cursor.column, c),
                    Action::MoveCursor(Direction::Right, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Enter => vec![
                    Action::InsertChar(
                        self.selection.cursor.line,
                        self.selection.cursor.column,
                        '\n',
                    ),
                    Action::MoveCursor(Direction::Down, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Backspace if self.buffer.contents.len_chars() > 0 => vec![
                    Action::DeleteChar(
                        self.selection.cursor.line,
                        self.selection.cursor.column - 1,
                    ),
                    Action::MoveCursor(Direction::Left, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Up => vec![
                    Action::MoveCursor(Direction::Up, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Down => vec![
                    Action::MoveCursor(Direction::Down, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Left => vec![
                    Action::MoveCursor(Direction::Left, 1),
                    Action::ReduceSelection,
                ],
                KeyCode::Right => vec![
                    Action::MoveCursor(Direction::Right, 1),
                    Action::ReduceSelection,
                ],
                _ => Vec::new(),
            }
        } else if modifiers == KeyModifiers::SHIFT {
            match code {
                KeyCode::Up => vec![Action::MoveCursor(Direction::Up, 1)],
                KeyCode::Down => vec![Action::MoveCursor(Direction::Down, 1)],
                KeyCode::Left => vec![Action::MoveCursor(Direction::Left, 1)],
                KeyCode::Right => vec![Action::MoveCursor(Direction::Right, 1)],
                _ => Vec::new(),
            }
        } else if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('c') => vec![Action::Quit],
                _ => Vec::new(),
            }
        } else if modifiers == KeyModifiers::ALT {
            match code {
                KeyCode::Char(';') => vec![Action::FlipSelection],
                _ => Vec::new(),
            }
        } else {
            Vec::new()
        })
    }

    fn handle_mouse_event(&self, mouse_event: MouseEvent) -> Result<Vec<Action>> {
        Ok(match mouse_event {
            _ => Vec::new(),
        })
    }

    fn handle_resize_event(&self, _width: u16, _height: u16) -> Result<Vec<Action>> {
        Ok(Vec::new())
    }

    pub fn apply_action(&mut self, action: Action) {
        match action {
            Action::Quit => {
                self.quit = true;
            }
            Action::InsertChar(line, column, character) => {
                self.buffer.insert_char(line, column, character);
            }
            Action::DeleteChar(line, column) => {
                self.buffer.delete_char(line, column);
            }
            Action::MoveCursor(direction, distance) => match direction {
                Direction::Up => {
                    self.selection.cursor.move_up(distance);
                }
                Direction::Down => {
                    self.selection.cursor.move_down(distance);
                }
                Direction::Left => {
                    self.selection.cursor.move_left(distance);
                }
                Direction::Right => {
                    self.selection.cursor.move_right(distance);
                }
            },
            Action::MoveAnchor(direction, distance) => match direction {
                Direction::Up => {
                    self.selection.anchor.move_up(distance);
                }
                Direction::Down => {
                    self.selection.anchor.move_down(distance);
                }
                Direction::Left => {
                    self.selection.anchor.move_left(distance);
                }
                Direction::Right => {
                    self.selection.anchor.move_right(distance);
                }
            },
            Action::ReduceSelection => {
                self.selection.reduce();
            }
            Action::FlipSelection => {
                self.selection.flip();
            }
        }
    }

    pub fn cursor_to_char(&self) -> usize {
        let Position { line, column } = self.selection.cursor;
        self.buffer.contents.line_to_char(line) + column
    }

    pub fn anchor_to_char(&self) -> usize {
        let Position { line, column } = self.selection.anchor;
        self.buffer.contents.line_to_char(line) + column
    }

    pub fn render(&self) {
        Terminal::clear();

        self.render_buffer();
        self.render_selection();
        self.render_status();

        Terminal::set_title(&self.title);

        Terminal::flush();
    }

    pub fn render_buffer(&self) {
        let (_, terminal_lines) = Terminal::size();
        let mut line_number = 0;
        for line in self.buffer.contents.lines() {
            if (line_number + 1) > terminal_lines {
                break;
            }
            Terminal::move_cursor_to(0, line_number);
            Terminal::print(&Cow::from(line));
            Terminal::print("\r");
            line_number += 1;
        }
    }

    pub fn render_selection(&self) {
        let cursor_line = self.selection.cursor.line;
        let cursor_column = self.selection.cursor.column;

        let anchor_line = self.selection.anchor.line;
        let anchor_column = self.selection.anchor.column;

        // Anchor
        Terminal::move_cursor_to(anchor_column as u16, anchor_line as u16);

        let anchor_char_index = self.anchor_to_char();

        let anchor_char = if self.buffer.contents.len_chars() > anchor_char_index {
            self.buffer.contents.char(anchor_char_index).to_string()
        } else {
            String::from(" ")
        };

        Terminal::print_styled(anchor_char.on_yellow());

        // Cursor
        Terminal::move_cursor_to(cursor_column as u16, cursor_line as u16);

        let cursor_char_index = self.cursor_to_char();

        let cursor_char = if self.buffer.contents.len_chars() > cursor_char_index {
            self.buffer.contents.char(cursor_char_index).to_string()
        } else {
            String::from(" ")
        };

        Terminal::print_styled(cursor_char.white().on_red());
    }

    pub fn render_status(&self) {
        let (_, terminal_lines) = Terminal::size();
        Terminal::move_cursor_to(0, terminal_lines);
        Terminal::print(&format!(
            "mode: {:?}, cursor: ({}, {}), anchor: ({}, {})",
            self.mode,
            self.selection.cursor.line,
            self.selection.cursor.column,
            self.selection.anchor.line,
            self.selection.anchor.column,
        ));
    }
}
