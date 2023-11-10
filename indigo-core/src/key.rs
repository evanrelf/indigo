use std::collections::HashSet;
use std::str::FromStr;
use winnow::{
    combinator::{alt, dispatch, fail, success},
    prelude::*,
    token::{any, one_of},
};

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
    dispatch! { any;
        's' => success(KeyModifier::Shift),
        'c' => success(KeyModifier::Ctrl),
        'a' => success(KeyModifier::Alt),
        _ => fail,
    }
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
        "backspace".value(KeyCode::Backspace),
        "enter".value(KeyCode::Enter),
        "left".value(KeyCode::Left),
        "right".value(KeyCode::Right),
        "up".value(KeyCode::Up),
        "down".value(KeyCode::Down),
        "tab".value(KeyCode::Tab),
        "escape".value(KeyCode::Escape),
        one_of(' '..='~').map(KeyCode::Char),
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
        assert_eq!("c".parse(), Ok(KeyModifier::Ctrl));
        assert_eq!("a".parse(), Ok(KeyModifier::Alt));
        assert!("abc".parse::<KeyModifier>().is_err());
    }

    #[test]
    fn test_parse_key_code() {
        assert_eq!(" ".parse(), Ok(KeyCode::Char(' ')));
        assert_eq!("!".parse(), Ok(KeyCode::Char('!')));
        assert_eq!("+".parse(), Ok(KeyCode::Char('+')));
        assert_eq!("~".parse(), Ok(KeyCode::Char('~')));
        assert_eq!("b".parse(), Ok(KeyCode::Char('b')));
        assert_eq!("backspace".parse(), Ok(KeyCode::Backspace));
        assert_eq!("escape".parse(), Ok(KeyCode::Escape));
        assert!("∆".parse::<KeyCode>().is_err());
    }
}
