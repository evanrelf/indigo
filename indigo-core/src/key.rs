use std::collections::HashSet;
use std::str::FromStr;
use winnow::{combinator::alt, prelude::*, token::one_of};

// TODO: Strict mode? Would forbid alternatives, abbreviations, repeated modifiers, incorrectly
// ordered modifiers, etc.

pub struct Key {
    pub modifiers: HashSet<KeyModifier>,
    pub code: KeyCode,
}

impl FromStr for Key {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| e.to_string())
    }
}

pub fn key(input: &mut &str) -> PResult<Key> {
    todo!()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key_modifier() {
        assert_eq!(key_modifier.parse("s"), Ok(KeyModifier::Shift));
        assert_eq!(key_modifier.parse("shift"), Ok(KeyModifier::Shift));
        assert_eq!(key_modifier.parse("c"), Ok(KeyModifier::Ctrl));
        assert_eq!(key_modifier.parse("ctrl"), Ok(KeyModifier::Ctrl));
        assert_eq!(key_modifier.parse("control"), Ok(KeyModifier::Ctrl));
        assert_eq!(key_modifier.parse("a"), Ok(KeyModifier::Alt));
        assert_eq!(key_modifier.parse("alt"), Ok(KeyModifier::Alt));
        assert_eq!(key_modifier.parse("option"), Ok(KeyModifier::Alt));
        assert!(key_modifier.parse("abc").is_err());
    }

    #[test]
    fn test_parse_key_code() {
        assert_eq!(key_code.parse(" "), Ok(KeyCode::Char(' ')));
        assert_eq!(key_code.parse("!"), Ok(KeyCode::Char('!')));
        assert_eq!(key_code.parse("+"), Ok(KeyCode::Char('+')));
        assert_eq!(key_code.parse("~"), Ok(KeyCode::Char('~')));
        assert_eq!(key_code.parse("b"), Ok(KeyCode::Char('b')));
        assert_eq!(key_code.parse("bs"), Ok(KeyCode::Backspace));
        assert_eq!(key_code.parse("backspace"), Ok(KeyCode::Backspace));
        assert_eq!(key_code.parse("escape"), Ok(KeyCode::Escape));
        assert_eq!(key_code.parse("esc"), Ok(KeyCode::Escape));
        assert!(key_code.parse("∆").is_err());
    }
}
