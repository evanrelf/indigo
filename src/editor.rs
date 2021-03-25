use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::Result;
use ropey::Rope;
use std::borrow::Cow;

pub enum Action {
    Quit,
    InsertChar(usize, usize, char),
    DeleteChar(usize, usize),
}

impl Action {
    pub fn apply(self, editor: &mut Editor) {
        match self {
            Action::Quit => {
                editor.quit = true;
            }
            Action::InsertChar(line, column, c) => {
                let index = editor.buffer.line_to_char(line) + column;
                editor.buffer.insert_char(index, c);
            }
            Action::DeleteChar(line, column) => {
                let index = editor.buffer.line_to_char(line) + column;
                editor.buffer.remove(index..(index + 1));
            }
        }
    }
}

pub struct Editor {
    pub quit: bool,
    pub title: String,
    pub buffer: Rope,
}

impl Editor {
    pub fn new() -> Self {
        Editor {
            quit: false,
            title: String::from("ind"),
            buffer: Rope::new(),
        }
    }

    pub fn run(&mut self) {
        Terminal::enter();
        Terminal::hide_cursor();
        Terminal::flush();

        while !self.quit {
            self.render();
            if let Some(action) = self.handle_event().unwrap() {
                action.apply(self);
            }
        }

        Terminal::exit();
    }

    pub fn handle_event(&mut self) -> Result<Option<Action>> {
        match event::read()? {
            Event::Key(key_event) => self.handle_key_event(key_event),
            Event::Mouse(mouse_event) => self.handle_mouse_event(mouse_event),
            Event::Resize(width, height) => self.handle_resize_event(width, height),
        }
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) -> Result<Option<Action>> {
        let KeyEvent { code, modifiers } = key_event;

        if modifiers == KeyModifiers::NONE {
            let length = self.buffer.len_chars();
            return Ok(match code {
                KeyCode::Char(c) => Some(Action::InsertChar(0, length, c)),
                KeyCode::Enter => Some(Action::InsertChar(0, length, '\n')),
                KeyCode::Backspace => {
                    if length > 0 {
                        Some(Action::DeleteChar(0, length - 1))
                    } else {
                        None
                    }
                }
                _ => None,
            });
        }

        if modifiers == KeyModifiers::CONTROL {
            return Ok(match code {
                KeyCode::Char('c') => Some(Action::Quit),
                _ => None,
            });
        }

        Ok(None)
    }

    fn handle_mouse_event(&mut self, mouse_event: MouseEvent) -> Result<Option<Action>> {
        Ok(match mouse_event {
            _ => None,
        })
    }

    fn handle_resize_event(&mut self, _width: u16, _height: u16) -> Result<Option<Action>> {
        Ok(None)
    }

    pub fn render(&mut self) {
        Terminal::clear();

        let mut line_number = 0;
        for line in self.buffer.lines() {
            Terminal::move_cursor_to(0, line_number);
            Terminal::print(&Cow::from(line));
            Terminal::print("\r");
            line_number += 1;
        }

        Terminal::set_title(&self.title);

        Terminal::flush();
    }
}
