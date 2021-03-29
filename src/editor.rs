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

pub struct Viewport {
    lines: u16,
    columns: u16,
    vertical_offset: usize,
    horizontal_offset: usize,
}

#[derive(Debug)]
pub enum Mode {
    Insert,
    Normal,
}

pub struct Editor {
    pub quit: bool,
    pub title: String,
    pub viewport: Viewport,
    pub mode: Mode,
    pub selections: Vec<Selection>,
    pub buffer: Buffer,
}

impl Editor {
    pub fn empty() -> Editor {
        let (columns, lines) = Terminal::size();

        Editor {
            quit: false,
            title: String::from("ind"),
            viewport: Viewport {
                lines,
                columns,
                vertical_offset: 0,
                horizontal_offset: 0,
            },
            mode: Mode::Normal,
            selections: vec![Selection::new()],
            buffer: Buffer::new(Rope::new()),
        }
    }

    pub fn from_file<P>(path: P) -> Editor
    where
        P: AsRef<Path>,
    {
        let (columns, lines) = Terminal::size();

        Editor {
            quit: false,
            title: String::from("ind"),
            viewport: Viewport {
                lines,
                columns,
                vertical_offset: 0,
                horizontal_offset: 0,
            },
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
            Event::Resize(lines, columns) => self.handle_resize_event(lines, columns),
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
        let kind = mouse_event.kind;
        let line = mouse_event.row as usize + self.viewport.vertical_offset;
        let column = mouse_event.column as usize;
        Ok(match kind {
            MouseEventKind::Down(MouseButton::Left) => vec![
                Action::MoveCursorAbsolute(0, line, column),
                Action::ReduceSelection(0),
            ],
            MouseEventKind::Drag(MouseButton::Left) | MouseEventKind::Down(MouseButton::Right) => {
                vec![Action::MoveCursorAbsolute(0, line, column)]
            }
            MouseEventKind::ScrollDown => vec![Action::ScrollDown(3)],
            MouseEventKind::ScrollUp => vec![Action::ScrollUp(3)],
            _ => vec![],
        })
    }

    fn handle_resize_event(&self, lines: u16, columns: u16) -> Result<Vec<Action>> {
        Ok(vec![Action::Resize(lines, columns)])
    }

    pub fn apply_action(&mut self, action: Action) {
        match action {
            Action::Quit => self.quit = true,
            Action::Resize(lines, columns) => {
                self.viewport.lines = lines;
                self.viewport.columns = columns;
            }
            Action::ScrollUp(amount) => {
                self.viewport.vertical_offset =
                    self.viewport.vertical_offset.saturating_sub(amount);
            }
            Action::ScrollDown(amount) => {
                self.viewport.vertical_offset += min(
                    amount,
                    self.buffer.contents.len_lines() - self.viewport.vertical_offset,
                );
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
        let offset = self.viewport.vertical_offset;
        let buffer_end_index = self.buffer.contents.len_lines();

        for terminal_line_index in 0..(self.viewport.lines - 1) {
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
        let visible_lines = self.viewport.vertical_offset
            ..(self.viewport.vertical_offset + self.viewport.lines as usize);
        let visible_columns = 0..self.viewport.horizontal_offset.into();

        // Anchor
        let anchor_line = self.selections[index].anchor.line;
        let anchor_column = self.selections[index].anchor.column;
        if visible_lines.contains(&anchor_line) && visible_columns.contains(&anchor_column) {
            self.render_selection_end(anchor_line, anchor_column, false);
        }

        // Cursor
        let cursor_line = self.selections[index].cursor.line;
        let cursor_column = self.selections[index].cursor.column;
        if visible_lines.contains(&cursor_line) && visible_columns.contains(&cursor_column) {
            self.render_selection_end(cursor_line, cursor_column, true);
        }
    }

    pub fn render_selection_end(&self, line: usize, column: usize, is_cursor: bool) {
        if let Some(char_index) = self.buffer.coordinates_to_index(line, column) {
            Terminal::move_cursor_to(
                (column - self.viewport.vertical_offset) as u16,
                (line - self.viewport.horizontal_offset) as u16,
            );
            let character = if self.buffer.contents.len_chars() > char_index {
                self.buffer.contents.char(char_index).to_string()
            } else {
                String::from(" ")
            };

            if is_cursor {
                Terminal::print_styled(character.white().on_red());
            } else {
                Terminal::print_styled(character.black().on_yellow());
            }
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

        Terminal::move_cursor_to(0, self.viewport.lines);
        Terminal::print(&format!(
            "mode: {:?}, cursor 0: ({}, {}), anchor 0: ({}, {}), voffset: {}, index: {:?}, coords: {:?}",
            self.mode,
            self.selections[0].cursor.line,
            self.selections[0].cursor.column,
            self.selections[0].anchor.line,
            self.selections[0].anchor.column,
            self.viewport.vertical_offset,
            cursor_index,
            cursor_coords
        ));
        Terminal::clear_until_newline();
    }
}
