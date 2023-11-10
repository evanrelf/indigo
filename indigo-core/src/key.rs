use std::collections::HashSet;
use std::str::FromStr;
use winnow::{combinator::alt, prelude::*, token::one_of};

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KeyModifier {
    Shift,
    Ctrl,
    Alt,
}

pub fn key_modifier(input: &mut &str) -> PResult<KeyModifier> {
    alt((
        alt(("shift", "s")).value(KeyModifier::Shift),
        alt(("control", "ctrl", "c")).value(KeyModifier::Ctrl),
        alt(("option", "alt", "a")).value(KeyModifier::Alt),
    ))
    .parse_next(input)
}

impl FromStr for KeyModifier {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_modifier.parse(s).map_err(|e| e.to_string())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    alt((
        alt(("backspace", "bs")).value(KeyCode::Backspace),
        alt(("enter", "return", "cr")).value(KeyCode::Enter),
        "left".value(KeyCode::Left),
        "right".value(KeyCode::Right),
        "up".value(KeyCode::Up),
        "down".value(KeyCode::Down),
        "tab".value(KeyCode::Tab),
        one_of(' '..='~').map(KeyCode::Char),
        alt(("escape", "esc")).value(KeyCode::Escape),
    ))
    .parse_next(input)
}

impl FromStr for KeyCode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_code.parse(s).map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key_modifier() {
        assert_eq!("s".parse(), Ok(KeyModifier::Shift));
        assert_eq!("shift".parse(), Ok(KeyModifier::Shift));
        assert_eq!("c".parse(), Ok(KeyModifier::Ctrl));
        assert_eq!("ctrl".parse(), Ok(KeyModifier::Ctrl));
        assert_eq!("control".parse(), Ok(KeyModifier::Ctrl));
        assert_eq!("a".parse(), Ok(KeyModifier::Alt));
        assert_eq!("alt".parse(), Ok(KeyModifier::Alt));
        assert_eq!("option".parse(), Ok(KeyModifier::Alt));
        assert!("abc".parse::<KeyModifier>().is_err());
    }

    #[test]
    fn test_parse_key_code() {
        assert_eq!(" ".parse(), Ok(KeyCode::Char(' ')));
        assert_eq!("!".parse(), Ok(KeyCode::Char('!')));
        assert_eq!("+".parse(), Ok(KeyCode::Char('+')));
        assert_eq!("~".parse(), Ok(KeyCode::Char('~')));
        assert_eq!("b".parse(), Ok(KeyCode::Char('b')));
        assert_eq!("bs".parse(), Ok(KeyCode::Backspace));
        assert_eq!("backspace".parse(), Ok(KeyCode::Backspace));
        assert_eq!("escape".parse(), Ok(KeyCode::Escape));
        assert_eq!("esc".parse(), Ok(KeyCode::Escape));
        assert!("∆".parse::<KeyCode>().is_err());
    }
}
