#![allow(dead_code)]

use crate::buffer::Buffer;
use crate::selection::{Position, Selection};
use crate::terminal::Terminal;
use crossterm::event::{
    self, Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};
use crossterm::style::Colorize;
use crossterm::Result;
use ropey::Rope;
use std::borrow::Cow;
use std::cmp::min;
use std::path::Path;

pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub enum Action {
    Quit,
    Resize(u16, u16),
    ScrollUp(usize),
    ScrollDown(usize),
    ChangeMode(Mode),
    InsertChar(usize, usize, char),
    DeleteChar(usize, usize),
    MoveCursorRelative(usize, Direction, usize),
    MoveCursorAbsolute(usize, usize, usize),
    MoveAnchorRelative(usize, Direction, usize),
    MoveAnchorAbsolute(usize, usize, usize),
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
    pub terminal_lines: u16,
    pub terminal_columns: u16,
    pub line_at_top: usize,
    pub mode: Mode,
    pub selections: Vec<Selection>,
    pub buffer: Buffer,
}

impl Editor {
    pub fn empty() -> Editor {
        let (terminal_columns, terminal_lines) = Terminal::size();

        Editor {
            quit: false,
            title: String::from("ind"),
            terminal_lines,
            terminal_columns,
            line_at_top: 0,
            mode: Mode::Normal,
            selections: vec![Selection::new()],
            buffer: Buffer::new(Rope::new()),
        }
    }

    pub fn from_file<P>(path: P) -> Editor
    where
        P: AsRef<Path>,
    {
        let (terminal_columns, terminal_lines) = Terminal::size();

        Editor {
            quit: false,
            title: String::from("ind"),
            terminal_lines,
            terminal_columns,
            line_at_top: 0,
            mode: Mode::Normal,
            selections: vec![Selection::new()],
            buffer: Buffer::from_file(path),
        }
    }

    pub fn run(mut self) {
        Terminal::enter();
        Terminal::hide_cursor();
        Terminal::enable_mouse_capture();
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
            Event::Resize(terminal_columns, terminal_lines) => {
                self.handle_resize_event(terminal_lines, terminal_columns)
            }
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
                    Action::MoveCursorRelative(0, Direction::Left, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('j') => Some(vec![
                    Action::MoveCursorRelative(0, Direction::Down, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('k') => Some(vec![
                    Action::MoveCursorRelative(0, Direction::Up, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Char('l') => Some(vec![
                    Action::MoveCursorRelative(0, Direction::Right, 1),
                    Action::ReduceSelection(0),
                ]),
                _ => None,
            }
        } else if modifiers == KeyModifiers::SHIFT {
            match code {
                KeyCode::Char('H') => Some(vec![Action::MoveCursorRelative(0, Direction::Left, 1)]),
                KeyCode::Char('J') => Some(vec![Action::MoveCursorRelative(0, Direction::Down, 1)]),
                KeyCode::Char('K') => Some(vec![Action::MoveCursorRelative(0, Direction::Up, 1)]),
                KeyCode::Char('L') => {
                    Some(vec![Action::MoveCursorRelative(0, Direction::Right, 1)])
                }
                _ => None,
            }
        } else if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('e') => Some(vec![Action::ScrollDown(1)]),
                KeyCode::Char('y') => Some(vec![Action::ScrollUp(1)]),
                _ => None,
            }
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
                    Action::MoveCursorRelative(0, Direction::Right, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Enter => Some(vec![
                    Action::InsertChar(cursor.line, cursor.column, '\n'),
                    Action::MoveCursorRelative(0, Direction::Down, 1),
                    Action::ReduceSelection(0),
                ]),
                KeyCode::Backspace if cursor.column > 0 => Some(vec![
                    Action::DeleteChar(cursor.line, cursor.column - 1),
                    Action::MoveCursorRelative(0, Direction::Left, 1),
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
        let MouseEvent {
            kind, row, column, ..
        } = mouse_event;
        Ok(match kind {
            MouseEventKind::Down(MouseButton::Left) => vec![
                Action::MoveCursorAbsolute(0, row.into(), column.into()),
                Action::ReduceSelection(0),
            ],
            MouseEventKind::Drag(MouseButton::Left) | MouseEventKind::Down(MouseButton::Right) => {
                vec![Action::MoveCursorAbsolute(0, row.into(), column.into())]
            }
            MouseEventKind::ScrollDown => vec![Action::ScrollDown(3)],
            MouseEventKind::ScrollUp => vec![Action::ScrollUp(3)],
            _ => vec![],
        })
    }

    fn handle_resize_event(
        &self,
        terminal_lines: u16,
        terminal_columns: u16,
    ) -> Result<Vec<Action>> {
        Ok(vec![Action::Resize(terminal_lines, terminal_columns)])
    }

    pub fn apply_action(&mut self, action: Action) {
        match action {
            Action::Quit => self.quit = true,
            Action::Resize(terminal_lines, terminal_columns) => {
                self.terminal_lines = terminal_lines;
                self.terminal_columns = terminal_columns;
            }
            Action::ScrollUp(amount) => {
                self.line_at_top -= min(amount, self.line_at_top);
            }
            Action::ScrollDown(amount) => {
                self.line_at_top +=
                    min(amount, self.buffer.contents.len_lines() - self.line_at_top);
            }
            Action::ChangeMode(mode) => self.mode = mode,
            Action::InsertChar(line, column, character) => {
                self.buffer.insert_char(line, column, character)
            }
            Action::DeleteChar(line, column) => self.buffer.delete_char(line, column),
            Action::MoveCursorRelative(index, direction, distance) => {
                match direction {
                    Direction::Up => self.selections[index].cursor.move_up(distance),
                    Direction::Down => self.selections[index].cursor.move_down(distance),
                    Direction::Left => self.selections[index].cursor.move_left(distance),
                    Direction::Right => self.selections[index].cursor.move_right(distance),
                };
            }
            Action::MoveCursorAbsolute(index, line, column) => {
                self.selections[index].cursor.line = line;
                self.selections[index].cursor.column = column;
            }
            Action::MoveAnchorRelative(index, direction, distance) => {
                match direction {
                    Direction::Up => self.selections[index].anchor.move_up(distance),
                    Direction::Down => self.selections[index].anchor.move_down(distance),
                    Direction::Left => self.selections[index].anchor.move_left(distance),
                    Direction::Right => self.selections[index].anchor.move_right(distance),
                };
            }
            Action::MoveAnchorAbsolute(index, line, column) => {
                self.selections[index].anchor.line = line;
                self.selections[index].anchor.column = column;
            }
            Action::ReduceSelection(index) => self.selections[index].reduce(),
            Action::FlipSelection(index) => self.selections[index].flip(),
        }
    }

    pub fn render(&self) {
        self.render_buffer();
        self.render_selections();
        self.render_status();

        Terminal::set_title(&self.title);

        Terminal::flush();
    }

    pub fn render_buffer(&self) {
        let offset = self.line_at_top;
        let buffer_end_index = self.buffer.contents.len_lines();

        for terminal_line_index in 0..self.terminal_lines {
            Terminal::move_cursor_to(0, terminal_line_index);
            Terminal::clear_line();
            let buffer_line_index = (terminal_line_index as usize) + offset;
            if buffer_line_index < buffer_end_index {
                let buffer_line_slice = self.buffer.contents.line(buffer_line_index);
                Terminal::print(&Cow::from(buffer_line_slice));
            } else {
                Terminal::print("~");
            }
        }
    }

    pub fn render_selections(&self) {
        for index in 0..self.selections.len() {
            self.render_selection(index);
        }
    }

    pub fn render_selection(&self, index: usize) {
        // Anchor
        let anchor_line = self.selections[index].anchor.line;
        let anchor_column = self.selections[index].anchor.column;

        if let Some(anchor_char_index) =
            self.buffer.coordinates_to_index(anchor_line, anchor_column)
        {
            Terminal::move_cursor_to(anchor_column as u16, anchor_line as u16);
            let anchor_char = if self.buffer.contents.len_chars() > anchor_char_index {
                self.buffer.contents.char(anchor_char_index).to_string()
            } else {
                String::from(" ")
            };
            Terminal::print_styled(anchor_char.on_yellow());
        }

        // Cursor
        let cursor_line = self.selections[index].cursor.line;
        let cursor_column = self.selections[index].cursor.column;

        if let Some(cursor_char_index) =
            self.buffer.coordinates_to_index(cursor_line, cursor_column)
        {
            Terminal::move_cursor_to(cursor_column as u16, cursor_line as u16);
            let cursor_char = if self.buffer.contents.len_chars() > cursor_char_index {
                self.buffer.contents.char(cursor_char_index).to_string()
            } else {
                String::from(" ")
            };

            Terminal::print_styled(cursor_char.white().on_red());
        }
    }

    pub fn render_status(&self) {
        let line = self.selections[0].cursor.line;
        let column = self.selections[0].cursor.column;
        let cursor_index = self.buffer.coordinates_to_index(line, column);
        let cursor_coords = if let Some(ix) = cursor_index {
            self.buffer.index_to_coordinates(ix)
        } else {
            (42, 42)
        };

        let (_, terminal_lines) = Terminal::size();
        Terminal::move_cursor_to(0, terminal_lines);
        Terminal::print(&format!(
            "mode: {:?}, cursor 0: ({}, {}), anchor 0: ({}, {}), top: {}, index: {:?}, coords: {:?}",
            self.mode,
            self.selections[0].cursor.line,
            self.selections[0].cursor.column,
            self.selections[0].anchor.line,
            self.selections[0].anchor.column,
            self.line_at_top,
            cursor_index,
            cursor_coords
        ));
        Terminal::clear_until_newline();
    }
}
