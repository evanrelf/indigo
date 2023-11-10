use std::collections::HashSet;
use std::str::FromStr;
use winnow::prelude::*;

pub struct Key {
    pub modifiers: HashSet<KeyModifier>,
    pub code: KeyCode,
}

pub fn key(input: &mut &str) -> PResult<Key> {
    todo!()
}

impl FromStr for Key {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| e.to_string())
    }
}

pub enum KeyModifier {
    Shift,
    Ctrl,
    Alt,
    Super,
}

pub fn key_modifier(input: &mut &str) -> PResult<KeyModifier> {
    todo!()
}

impl FromStr for KeyModifier {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_modifier.parse(s).map_err(|e| e.to_string())
    }
}

pub enum KeyCode {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Tab,
    Char(char),
    Escape,
}

pub fn key_code(input: &mut &str) -> PResult<KeyCode> {
    todo!()
}

impl FromStr for KeyCode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_code.parse(s).map_err(|e| e.to_string())
    }
}
