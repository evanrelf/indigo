use std::collections::HashSet;
use std::str::FromStr;
use winnow::{
    combinator::{alt, repeat, terminated},
    prelude::*,
    token::one_of,
};

// TODO: Strict mode? Would forbid alternatives, abbreviations, repeated modifiers, incorrectly
// ordered modifiers, etc.

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Key {
    // TODO: Use `bitflags` or `flagset` crate
    pub modifiers: HashSet<KeyModifier>,
    pub code: KeyCode,
}

impl<C> From<C> for Key
where
    C: Into<KeyCode>,
{
    fn from(code: C) -> Self {
        Self {
            modifiers: HashSet::new(),
            code: code.into(),
        }
    }
}

impl<M, C> From<(M, C)> for Key
where
    M: Into<HashSet<KeyModifier>>,
    C: Into<KeyCode>,
{
    fn from((modifiers, code): (M, C)) -> Self {
        Self {
            modifiers: modifiers.into(),
            code: code.into(),
        }
    }
}

impl FromStr for Key {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| e.to_string())
    }
}

pub fn key(input: &mut &str) -> PResult<Key> {
    alt((key_wrapped, key_bare)).parse_next(input)
}

pub fn key_wrapped(input: &mut &str) -> PResult<Key> {
    let _ = "<".parse_next(input)?;
    let modifiers: Vec<_> = repeat(0.., terminated(key_modifier, "-")).parse_next(input)?;
    let modifiers = modifiers.into_iter().collect();
    let code = key_code.parse_next(input)?;
    let _ = ">".parse_next(input)?;
    Ok(Key { modifiers, code })
}

pub fn key_bare(input: &mut &str) -> PResult<Key> {
    let modifiers = HashSet::new();
    let code = key_code_bare.parse_next(input)?;
    Ok(Key { modifiers, code })
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
    Escape,
    Char(char),
}

pub fn key_code(input: &mut &str) -> PResult<KeyCode> {
    alt((key_code_wrapped, key_code_bare)).parse_next(input)
}

pub fn key_code_wrapped(input: &mut &str) -> PResult<KeyCode> {
    alt((
        alt(("backspace", "bs")).value(KeyCode::Backspace),
        alt(("enter", "return", "cr")).value(KeyCode::Enter),
        "left".value(KeyCode::Left),
        "right".value(KeyCode::Right),
        "up".value(KeyCode::Up),
        "down".value(KeyCode::Down),
        "tab".value(KeyCode::Tab),
        alt(("escape", "esc")).value(KeyCode::Escape),
        "lt".value(KeyCode::Char('<')),
        "gt".value(KeyCode::Char('>')),
    ))
    .parse_next(input)
}

pub fn key_code_bare(input: &mut &str) -> PResult<KeyCode> {
    one_of(' '..='~').map(KeyCode::Char).parse_next(input)
}

impl From<char> for KeyCode {
    fn from(c: char) -> Self {
        Self::Char(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(key.parse("<"), Ok(Key::from('<')));
        assert_eq!(key.parse(">"), Ok(Key::from('>')));
        assert_eq!(key.parse("a"), Ok(Key::from('a')));
        assert_eq!(key.parse("<s-a>"), Ok(Key::from(([Shift], 'a'))));
        assert_eq!(key.parse("<c-a>"), Ok(Key::from(([Ctrl], 'a'))));
        assert_eq!(key.parse("<c-s-a>"), Ok(Key::from(([Ctrl, Shift], 'a'))));
        assert_eq!(key.parse("<tab>"), Ok(Key::from(Tab)));
        assert_eq!(key.parse("<c-s-tab>"), Ok(Key::from(([Ctrl, Shift], Tab))));
        assert_eq!(key.parse("<c-c-tab>"), Ok(Key::from(([Ctrl], Tab))));
        assert!(key.parse("tab").is_err());
    }

    #[test]
    fn test_parse_keys() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(
            repeat(0.., key).parse("%s\\bfn\\b<enter>E<a-k>test<enter><"),
            Ok(vec![
                Key::from('%'),
                Key::from('s'),
                Key::from('\\'),
                Key::from('b'),
                Key::from('f'),
                Key::from('n'),
                Key::from('\\'),
                Key::from('b'),
                Key::from(Enter),
                Key::from('E'),
                Key::from(([Alt], 'k')),
                Key::from('t'),
                Key::from('e'),
                Key::from('s'),
                Key::from('t'),
                Key::from(Enter),
                Key::from('<'),
            ])
        );
    }

    #[test]
    fn test_parse_key_modifier() {
        use KeyModifier::*;
        assert_eq!(key_modifier.parse("s"), Ok(Shift));
        assert_eq!(key_modifier.parse("shift"), Ok(Shift));
        assert_eq!(key_modifier.parse("c"), Ok(Ctrl));
        assert_eq!(key_modifier.parse("ctrl"), Ok(Ctrl));
        assert_eq!(key_modifier.parse("control"), Ok(Ctrl));
        assert_eq!(key_modifier.parse("a"), Ok(Alt));
        assert_eq!(key_modifier.parse("alt"), Ok(Alt));
        assert_eq!(key_modifier.parse("option"), Ok(Alt));
        assert!(key_modifier.parse("abc").is_err());
    }

    #[test]
    fn test_parse_key_code() {
        use KeyCode::*;
        assert_eq!(key_code.parse(" "), Ok(Char(' ')));
        assert_eq!(key_code.parse("!"), Ok(Char('!')));
        assert_eq!(key_code.parse("+"), Ok(Char('+')));
        assert_eq!(key_code.parse("~"), Ok(Char('~')));
        assert_eq!(key_code.parse("b"), Ok(Char('b')));
        assert_eq!(key_code.parse("B"), Ok(Char('B')));
        assert_eq!(key_code.parse("bs"), Ok(Backspace));
        assert_eq!(key_code.parse("backspace"), Ok(Backspace));
        assert_eq!(key_code.parse("escape"), Ok(Escape));
        assert_eq!(key_code.parse("esc"), Ok(Escape));
        assert!(key_code.parse("∆").is_err());
    }
}
