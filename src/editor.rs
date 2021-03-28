use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::style::{Color, Colorize};
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
}

pub struct Cursor {
    line: usize,
    column: usize,
}

pub struct Editor {
    pub quit: bool,
    pub title: String,
    pub cursor: Cursor,
    pub buffer: Rope,
}

impl Editor {
    pub fn new() -> Self {
        Editor {
            quit: false,
            title: String::from("ind"),
            cursor: Cursor { line: 0, column: 0 },
            buffer: Rope::new(),
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
                    Action::InsertChar(self.cursor.line, self.cursor.column, c),
                    Action::MoveCursor(Direction::Right, 1),
                ],
                KeyCode::Enter => vec![
                    Action::InsertChar(self.cursor.line, self.cursor.column, '\n'),
                    Action::MoveCursor(Direction::Down, 1),
                ],
                KeyCode::Backspace if self.buffer.len_chars() > 0 => vec![
                    Action::DeleteChar(self.cursor.line, self.cursor.column - 1),
                    Action::MoveCursor(Direction::Left, 1),
                ],
                _ => Vec::new(),
            }
        } else if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('c') => vec![Action::Quit],
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
            Action::InsertChar(line, column, c) => {
                let index = self.buffer.line_to_char(line) + column;
                self.buffer.insert_char(index, c);
            }
            Action::DeleteChar(line, column) => {
                let index = self.buffer.line_to_char(line) + column;
                self.buffer.remove(index..(index + 1));
            }
            Action::MoveCursor(direction, distance) => match direction {
                Direction::Up => self.cursor.line -= distance,
                Direction::Down => self.cursor.line += distance,
                Direction::Left => self.cursor.column -= distance,
                Direction::Right => self.cursor.column += distance,
            },
        }
    }

    pub fn cursor_to_char(&self) -> usize {
        self.buffer.line_to_char(self.cursor.line) + self.cursor.column
    }

    pub fn render(&self) {
        Terminal::clear();

        let mut line_number = 0;
        for line in self.buffer.lines() {
            Terminal::move_cursor_to(0, line_number);
            Terminal::print(&Cow::from(line));
            Terminal::print("\r");
            line_number += 1;
        }

        Terminal::move_cursor_to(self.cursor.column as u16, self.cursor.line as u16);
        let cursor_char = self.cursor_to_char();
        let character = if self.buffer.len_chars() > cursor_char {
            self.buffer.char(cursor_char).to_string()
        } else {
            String::from(" ")
        };
        Terminal::print_styled(character.white().on_black());

        Terminal::set_title(&format!(
            "ind (cursor: {},{})",
            self.cursor.line, self.cursor.column
        ));

        Terminal::flush();
    }
}
