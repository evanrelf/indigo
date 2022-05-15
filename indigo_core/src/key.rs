use bitflags::bitflags;
use std::collections::HashMap;

pub struct Key {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

pub enum KeyCode {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Tab,
    BackTab,
    Char(char),
    Esc,
}

bitflags! {
    pub struct KeyModifiers: u8 {
        const SHIFT = 0b0000_0001;
        const CONTROL = 0b0000_0010;
        const ALT = 0b0000_0100;
        const NONE = 0b0000_0000;
    }
}

pub type KeyMap = HashMap<Key, Todo>;

pub enum Todo {}
