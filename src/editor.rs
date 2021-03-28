#![allow(dead_code)]

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
    ChangeMode(Mode),
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
            mode: Mode::Normal,
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
                    Action::MoveCursor(Direction::Left, 1),
                    Action::ReduceSelection,
                ]),
                KeyCode::Char('j') => Some(vec![
                    Action::MoveCursor(Direction::Down, 1),
                    Action::ReduceSelection,
                ]),
                KeyCode::Char('k') => Some(vec![
                    Action::MoveCursor(Direction::Up, 1),
                    Action::ReduceSelection,
                ]),
                KeyCode::Char('l') => Some(vec![
                    Action::MoveCursor(Direction::Right, 1),
                    Action::ReduceSelection,
                ]),
                _ => None,
            }
        } else if modifiers == KeyModifiers::SHIFT {
            match code {
                KeyCode::Char('H') => Some(vec![Action::MoveCursor(Direction::Left, 1)]),
                KeyCode::Char('J') => Some(vec![Action::MoveCursor(Direction::Down, 1)]),
                KeyCode::Char('K') => Some(vec![Action::MoveCursor(Direction::Up, 1)]),
                KeyCode::Char('L') => Some(vec![Action::MoveCursor(Direction::Right, 1)]),
                _ => None,
            }
        } else if modifiers == KeyModifiers::CONTROL {
            None
        } else if modifiers == KeyModifiers::ALT {
            match code {
                KeyCode::Char(';') => Some(vec![Action::FlipSelection]),
                _ => None,
            }
        } else {
            None
        }
    }

    fn handle_key_event_insert_mode(&self, key_event: KeyEvent) -> Option<Vec<Action>> {
        let KeyEvent { code, .. } = key_event;
        let cursor = &self.selection.cursor;
        match code {
            KeyCode::Esc => Some(vec![Action::ChangeMode(Mode::Normal)]),
            KeyCode::Char(c) => Some(vec![
                Action::InsertChar(cursor.line, cursor.column, c),
                Action::MoveCursor(Direction::Right, 1),
                Action::ReduceSelection,
            ]),
            KeyCode::Enter => Some(vec![
                Action::InsertChar(cursor.line, cursor.column, '\n'),
                Action::MoveCursor(Direction::Down, 1),
                Action::ReduceSelection,
            ]),
            KeyCode::Backspace if cursor.column > 0 => Some(vec![
                Action::DeleteChar(cursor.line, cursor.column - 1),
                Action::MoveCursor(Direction::Left, 1),
                Action::ReduceSelection,
            ]),
            _ => None,
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
            Action::MoveCursor(direction, distance) => {
                match direction {
                    Direction::Up => self.selection.cursor.move_up(distance),
                    Direction::Down => self.selection.cursor.move_down(distance),
                    Direction::Left => self.selection.cursor.move_left(distance),
                    Direction::Right => self.selection.cursor.move_right(distance),
                };
            }
            Action::MoveAnchor(direction, distance) => {
                match direction {
                    Direction::Up => self.selection.anchor.move_up(distance),
                    Direction::Down => self.selection.anchor.move_down(distance),
                    Direction::Left => self.selection.anchor.move_left(distance),
                    Direction::Right => self.selection.anchor.move_right(distance),
                };
            }
            Action::ReduceSelection => self.selection.reduce(),
            Action::FlipSelection => self.selection.flip(),
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
