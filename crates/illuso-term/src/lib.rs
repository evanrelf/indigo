pub mod escape;
pub mod old;
pub mod tty;

use bitflags::bitflags;

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
