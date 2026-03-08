#![allow(unused)]

pub mod escape;
pub mod old;

use bitflags::bitflags;
use rustix::termios::{self, OptionalActions, Termios};
use std::io;

pub enum Event {
    Key(Key),
    Resize { height: u16, width: u16 },
}

pub struct Key {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

pub enum KeyCode {
    Esc,
    Backspace,
    Tab,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Char(char),
}

bitflags! {
    pub struct KeyModifiers: u8 {
        const SHIFT = 1;
        const ALT = 2;
        const CTRL = 4;
    }
}

pub struct Terminal {
    original_termios: Termios,
    in_band_resize: bool,
    sync_update: bool,
}

impl Terminal {
    fn init() -> io::Result<Self> {
        let original_termios = Self::enable_raw_mode()?;
        Ok(Self {
            original_termios,
            in_band_resize: false,
            sync_update: false,
        })
    }

    fn enable_raw_mode() -> io::Result<Termios> {
        let mut termios = termios::tcgetattr(io::stdin())?;
        termios.make_raw();
        termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;

        Ok(termios)
    }

    fn disable_raw_mode(termios: &Termios) -> io::Result<()> {
        termios::tcsetattr(io::stdin(), OptionalActions::Flush, termios)?;

        Ok(())
    }
}

#[must_use]
pub fn init() -> Terminal {
    Terminal::init().unwrap()
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = Self::disable_raw_mode(&self.original_termios);
    }
}
