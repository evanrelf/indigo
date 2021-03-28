#![allow(dead_code)]

use crate::buffer::Buffer;
use crate::selection::{Position, Selection};
use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::style::Colorize;
use crossterm::Result;
use ropey::Rope;
use std::borrow::Cow;
use std::path::Path;

pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub enum Action {
    Quit,
    ChangeMode(Mode),
    InsertChar(usize, usize, char),
    DeleteChar(usize, usize),
    MoveCursor(usize, Direction, usize),
    MoveAnchor(usize, Direction, usize),
    ReduceSelection(usize),
    FlipSelection(usize),
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
    pub selections: Vec<Selection>,
    pub buffer: Buffer,
}

impl Editor {
    pub fn empty() -> Editor {
        Editor {
            quit: false,
            title: String::from("ind"),
            mode: Mode::Normal,
            selections: vec![Selection::new()],
            buffer: Buffer::new(Rope::new()),
        }
    }

    pub fn from_file<P>(path: P) -> Editor
    where
        P: AsRef<Path>,
    {
        Editor {
            quit: false,
            title: String::from("ind"),
            mode: Mode::Normal,
            selections: vec![Selection::new()],
            buffer: Buffer::from_file(path),
        }
    }

    pub fn run(mut self) {
        Terminal::enter();
        Terminal::hide_cursor();
        Terminal::flush();

        self.render();
        while !self.quit {
            let actions = self.handle_event().unwrap();
            if !actions.is_empty() {
                for action in actions {
                    self.apply_action(action);
                }
                self.render();
            }
        }

        Terminal::exit();
    }

    pub fn handle_event(&self) -> Result<Vec<Action>> {
        match event::read()? {
            Event::Key(key_event) => Ok(self.handle_key_event(key_event)),
            Event::Mouse(mouse_event) => self.handle_mouse_event(mouse_event),
            Event::Resize(width, height) => self.handle_resize_event(width, height),
        }
    }

    fn handle_key_event(&self, key_event: KeyEvent) -> Vec<Action> {
        let mode_key = match self.mode {
            Mode::Normal => self.handle_key_event_normal_mode(key_event),
            Mode::Insert => self.handle_key_event_insert_mode(key_event),
        };

        mode_key
            .or(self.handle_key_event_any_mode(key_event))
            .unwrap_or(vec![])
    }

    fn handle_key_event_normal_mode(&self, key_event: KeyEvent) -> Option<Vec<Action>> {
        let KeyEvent { modifiers, code } = key_event;
        if modifiers == KeyModifiers::NONE {
            match code {
                KeyCode::Char('i') => Some(vec![Action::ChangeMode(Mode::Insert)]),
                KeyCode::Char('h') => Some(vec![
                    Action::MoveCursor(0, Direction::Left, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('j') => Some(vec![
                    Action::MoveCursor(0, Direction::Down, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('k') => Some(vec![
                    Action::MoveCursor(0, Direction::Up, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('l') => Some(vec![
                    Action::MoveCursor(0, Direction::Right, 1),
                    Action::ReduceSelection(0),
                ]),
                _ => None,
            }
        } else if modifiers == KeyModifiers::SHIFT {
            match code {
                KeyCode::Char('H') => Some(vec![Action::MoveCursor(0, Direction::Left, 1)]),
                KeyCode::Char('J') => Some(vec![Action::MoveCursor(0, Direction::Down, 1)]),
                KeyCode::Char('K') => Some(vec![Action::MoveCursor(0, Direction::Up, 1)]),
                KeyCode::Char('L') => Some(vec![Action::MoveCursor(0, Direction::Right, 1)]),
                _ => None,
            }
        } else if modifiers == KeyModifiers::CONTROL {
            None
        } else if modifiers == KeyModifiers::ALT {
            match code {
                KeyCode::Char(';') => Some(vec![Action::FlipSelection(0)]),
                _ => None,
            }
        } else {
            None
        }
    }

    fn handle_key_event_insert_mode(&self, key_event: KeyEvent) -> Option<Vec<Action>> {
        let KeyEvent { modifiers, code } = key_event;
        let cursor = &self.selections[0].cursor;
        if modifiers == KeyModifiers::NONE || modifiers == KeyModifiers::SHIFT {
            match code {
                KeyCode::Esc => Some(vec![Action::ChangeMode(Mode::Normal)]),
                KeyCode::Char(c) => Some(vec![
                    Action::InsertChar(cursor.line, cursor.column, c),
                    Action::MoveCursor(0, Direction::Right, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Enter => Some(vec![
                    Action::InsertChar(cursor.line, cursor.column, '\n'),
                    Action::MoveCursor(0, Direction::Down, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Backspace if cursor.column > 0 => Some(vec![
                    Action::DeleteChar(cursor.line, cursor.column - 1),
                    Action::MoveCursor(1, Direction::Left, 1),
                    Action::ReduceSelection(0),
                ]),
                _ => None,
            }
        } else {
            None
        }
    }

    fn handle_key_event_any_mode(&self, key_event: KeyEvent) -> Option<Vec<Action>> {
        let KeyEvent { modifiers, code } = key_event;
        if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('c') => Some(vec![Action::Quit]),
                _ => None,
            }
        } else {
            None
        }
    }

    fn handle_mouse_event(&self, mouse_event: MouseEvent) -> Result<Vec<Action>> {
        match mouse_event {
            _ => Ok(vec![]),
        }
    }

    fn handle_resize_event(&self, _width: u16, _height: u16) -> Result<Vec<Action>> {
        Ok(vec![])
    }

    pub fn apply_action(&mut self, action: Action) {
        match action {
            Action::Quit => self.quit = true,
            Action::ChangeMode(mode) => self.mode = mode,
            Action::InsertChar(line, column, character) => {
                self.buffer.insert_char(line, column, character)
            }
            Action::DeleteChar(line, column) => self.buffer.delete_char(line, column),
            Action::MoveCursor(index, direction, distance) => {
                match direction {
                    Direction::Up => self.selections[index].cursor.move_up(distance),
                    Direction::Down => self.selections[index].cursor.move_down(distance),
                    Direction::Left => self.selections[index].cursor.move_left(distance),
                    Direction::Right => self.selections[index].cursor.move_right(distance),
                };
            }
            Action::MoveAnchor(index, direction, distance) => {
                match direction {
                    Direction::Up => self.selections[index].anchor.move_up(distance),
                    Direction::Down => self.selections[index].anchor.move_down(distance),
                    Direction::Left => self.selections[index].anchor.move_left(distance),
                    Direction::Right => self.selections[index].anchor.move_right(distance),
                };
            }
            Action::ReduceSelection(index) => self.selections[index].reduce(),
            Action::FlipSelection(index) => self.selections[index].flip(),
        }
    }

    pub fn cursor_to_char(&self, index: usize) -> usize {
        let Position { line, column } = self.selections[index].cursor;
        self.buffer.contents.line_to_char(line) + column
    }

    pub fn anchor_to_char(&self, index: usize) -> usize {
        let Position { line, column } = self.selections[index].anchor;
        self.buffer.contents.line_to_char(line) + column
    }

    pub fn render(&self) {
        Terminal::clear();

        self.render_buffer();
        self.render_selections();
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

    pub fn render_selections(&self) {
        for index in 0..self.selections.len() {
            self.render_selection(index);
        }
    }

    pub fn render_selection(&self, index: usize) {
        let cursor_line = self.selections[index].cursor.line;
        let cursor_column = self.selections[index].cursor.column;

        let anchor_line = self.selections[index].anchor.line;
        let anchor_column = self.selections[index].anchor.column;

        // Anchor
        Terminal::move_cursor_to(anchor_column as u16, anchor_line as u16);

        let anchor_char_index = self.anchor_to_char(0);

        let anchor_char = if self.buffer.contents.len_chars() > anchor_char_index {
            self.buffer.contents.char(anchor_char_index).to_string()
        } else {
            String::from(" ")
        };

        Terminal::print_styled(anchor_char.on_yellow());

        // Cursor
        Terminal::move_cursor_to(cursor_column as u16, cursor_line as u16);

        let cursor_char_index = self.cursor_to_char(0);

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
            "mode: {:?}, cursor 0: ({}, {}), anchor 0: ({}, {})",
            self.mode,
            self.selections[0].cursor.line,
            self.selections[0].cursor.column,
            self.selections[0].anchor.line,
            self.selections[0].anchor.column,
        ));
    }
}
