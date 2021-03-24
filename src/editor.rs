use crate::terminal::Terminal;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent};
use crossterm::Result;
use ropey::Rope;
use std::borrow::Cow;

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
            self.handle_event().unwrap();
        }

        Terminal::exit();
    }

    pub fn handle_event(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) => self.handle_key_event(key_event),
            Event::Mouse(mouse_event) => self.handle_mouse_event(mouse_event),
            Event::Resize(width, height) => self.handle_resize_event(width, height),
        }
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) -> Result<()> {
        let KeyEvent { code, modifiers } = key_event;

        if modifiers == KeyModifiers::NONE {
            let length = self.buffer.len_chars();
            match code {
                KeyCode::Char(c) => {
                    self.buffer.insert_char(length, c);
                }
                KeyCode::Enter => {
                    self.buffer.insert(length, "\n");
                }
                KeyCode::Backspace => {
                    if length > 0 {
                        self.buffer.remove((length - 1)..length);
                    }
                }
                _ => (),
            }
        }

        if modifiers == KeyModifiers::CONTROL {
            match code {
                KeyCode::Char('c') => self.quit = true,
                _ => (),
            }
        }

        Ok(())
    }

    fn handle_mouse_event(&mut self, mouse_event: MouseEvent) -> Result<()> {
        match mouse_event {
            _ => Ok(()),
        }
    }

    fn handle_resize_event(&mut self, _width: u16, _height: u16) -> Result<()> {
        Ok(())
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
