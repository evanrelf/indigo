use flagset::{flags, FlagSet};
use itertools::Itertools as _;
use std::{
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    str::FromStr,
};
use winnow::{
    combinator::{alt, repeat, terminated},
    prelude::*,
    token::one_of,
};

// TODO: Strict mode? Would forbid repeated modifiers, incorrectly ordered modifiers, etc.

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Keys(pub Vec<Key>);

impl Display for Keys {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for key in &self.0 {
            write!(f, "{key}")?;
        }
        Ok(())
    }
}

impl FromStr for Keys {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        keys.parse(s).map_err(|e| e.to_string())
    }
}

fn keys(input: &mut &str) -> ModalResult<Keys> {
    repeat(0.., key).map(Keys).parse_next(input)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Key {
    pub modifiers: FlagSet<KeyModifier>,
    pub code: KeyCode,
}

impl<C> From<C> for Key
where
    C: Into<KeyCode>,
{
    fn from(code: C) -> Self {
        Self {
            modifiers: FlagSet::default(),
            code: code.into(),
        }
    }
}

impl<C> From<(KeyModifier, C)> for Key
where
    C: Into<KeyCode>,
{
    fn from((modifier, code): (KeyModifier, C)) -> Self {
        Self {
            modifiers: modifier.into(),
            code: code.into(),
        }
    }
}

impl<C> From<(KeyModifier, KeyModifier, C)> for Key
where
    C: Into<KeyCode>,
{
    fn from((modifier1, modifier2, code): (KeyModifier, KeyModifier, C)) -> Self {
        Self {
            modifiers: modifier1 | modifier2,
            code: code.into(),
        }
    }
}

impl<C> From<(KeyModifier, KeyModifier, KeyModifier, C)> for Key
where
    C: Into<KeyCode>,
{
    fn from(
        (modifier1, modifier2, modifier3, code): (KeyModifier, KeyModifier, KeyModifier, C),
    ) -> Self {
        Self {
            modifiers: modifier1 | modifier2 | modifier3,
            code: code.into(),
        }
    }
}

impl<const N: usize, C> From<([KeyModifier; N], C)> for Key
where
    C: Into<KeyCode>,
{
    fn from((modifiers, code): ([KeyModifier; N], C)) -> Self {
        Self {
            modifiers: modifiers.into_iter().fold(FlagSet::default(), |x, y| x | y),
            code: code.into(),
        }
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let code = match self.code {
            KeyCode::Backspace => "bs",
            KeyCode::Delete => "del",
            KeyCode::Return => "ret",
            KeyCode::Left => "left",
            KeyCode::Right => "right",
            KeyCode::Up => "up",
            KeyCode::Down => "down",
            KeyCode::Tab => "tab",
            KeyCode::Escape => "esc",
            KeyCode::Char(c) => &c.to_string(),
        };
        if self.modifiers.is_empty() && matches!(self.code, KeyCode::Char(_)) {
            write!(f, "{code}")?;
        } else {
            write!(f, "<")?;
            for modifier in self.modifiers.into_iter().sorted_unstable() {
                let modifier = match modifier {
                    KeyModifier::Control => "c",
                    KeyModifier::Alt => "a",
                    KeyModifier::Shift => "s",
                };
                write!(f, "{modifier}-")?;
            }
            write!(f, "{code}>")?;
        }
        Ok(())
    }
}

impl FromStr for Key {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| e.to_string())
    }
}

impl Hash for Key {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.modifiers.bits().hash(state);
        self.code.hash(state);
    }
}

fn key(input: &mut &str) -> ModalResult<Key> {
    alt((key_wrapped, key_bare)).parse_next(input)
}

fn key_wrapped(input: &mut &str) -> ModalResult<Key> {
    let _ = "<".parse_next(input)?;
    let modifiers: Vec<_> = repeat(0.., terminated(key_modifier, "-")).parse_next(input)?;
    let modifiers = modifiers.into_iter().fold(FlagSet::default(), |x, y| x | y);
    let code = key_code.parse_next(input)?;
    let _ = ">".parse_next(input)?;
    Ok(Key { modifiers, code })
}

fn key_bare(input: &mut &str) -> ModalResult<Key> {
    let modifiers = FlagSet::default();
    let code = key_code_bare.parse_next(input)?;
    Ok(Key { modifiers, code })
}

flags! {
    #[derive(Hash, Ord, PartialOrd)]
    pub enum KeyModifier: u8 {
        Control,
        Alt,
        Shift,
    }
}

fn key_modifier(input: &mut &str) -> ModalResult<KeyModifier> {
    alt((
        "c".value(KeyModifier::Control),
        "a".value(KeyModifier::Alt),
        "s".value(KeyModifier::Shift),
    ))
    .parse_next(input)
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum KeyCode {
    Backspace,
    Delete,
    Return,
    Left,
    Right,
    Up,
    Down,
    Tab,
    Escape,
    Char(char),
}

fn key_code(input: &mut &str) -> ModalResult<KeyCode> {
    alt((key_code_wrapped, key_code_bare)).parse_next(input)
}

fn key_code_wrapped(input: &mut &str) -> ModalResult<KeyCode> {
    alt((
        "bs".value(KeyCode::Backspace),
        "del".value(KeyCode::Delete),
        "ret".value(KeyCode::Return),
        "left".value(KeyCode::Left),
        "right".value(KeyCode::Right),
        "up".value(KeyCode::Up),
        "down".value(KeyCode::Down),
        "tab".value(KeyCode::Tab),
        "esc".value(KeyCode::Escape),
        "lt".value(KeyCode::Char('<')),
        "gt".value(KeyCode::Char('>')),
    ))
    .parse_next(input)
}

fn key_code_bare(input: &mut &str) -> ModalResult<KeyCode> {
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
    fn test_parse_keys() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(
            keys.parse("%s\\bfn\\b<ret>E<a-k>test<ret><"),
            Ok(Keys(vec![
                Key::from('%'),
                Key::from('s'),
                Key::from('\\'),
                Key::from('b'),
                Key::from('f'),
                Key::from('n'),
                Key::from('\\'),
                Key::from('b'),
                Key::from(Return),
                Key::from('E'),
                Key::from(([Alt], 'k')),
                Key::from('t'),
                Key::from('e'),
                Key::from('s'),
                Key::from('t'),
                Key::from(Return),
                Key::from('<'),
            ]))
        );
    }

    #[test]
    fn test_parse_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(key.parse("<"), Ok(Key::from('<')));
        assert_eq!(key.parse(">"), Ok(Key::from('>')));
        assert_eq!(key.parse("a"), Ok(Key::from('a')));
        // TODO: Distinguish between shift+a and A?
        assert_eq!(key.parse("<s-a>"), Ok(Key::from(([Shift], 'a'))));
        assert_eq!(key.parse("A"), Ok(Key::from(([], 'A'))));
        assert_eq!(key.parse("<c-a>"), Ok(Key::from((Control, 'a'))));
        assert_eq!(key.parse("<c-a>"), Ok(Key::from(([Control], 'a'))));
        assert_eq!(key.parse("<c-s-a>"), Ok(Key::from(([Control, Shift], 'a'))));
        assert_eq!(key.parse("<c-s-a>"), Ok(Key::from((Control, Shift, 'a'))));
        assert_eq!(key.parse("<tab>"), Ok(Key::from(Tab)));
        assert_eq!(
            key.parse("<c-s-tab>"),
            Ok(Key::from(([Control, Shift], Tab)))
        );
        assert_eq!(key.parse("<c-c-tab>"), Ok(Key::from(([Control], Tab))));
        assert!(key.parse("tab").is_err());
    }

    #[test]
    fn test_print_key() {
        assert_eq!(
            key.parse("<c-a-s-tab>").map(|k| k.to_string()).ok(),
            Some(String::from("<c-a-s-tab>"))
        );
        assert_eq!(
            key.parse("<s-c-a-tab>").map(|k| k.to_string()).ok(),
            Some(String::from("<c-a-s-tab>"))
        );
        assert_eq!(
            key.parse("<a-s-c-tab>").map(|k| k.to_string()).ok(),
            Some(String::from("<c-a-s-tab>"))
        );
    }

    #[test]
    fn test_parse_key_modifier() {
        use KeyModifier::*;
        assert_eq!(key_modifier.parse("c"), Ok(Control));
        assert_eq!(key_modifier.parse("a"), Ok(Alt));
        assert_eq!(key_modifier.parse("s"), Ok(Shift));
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
        assert_eq!(key_code.parse("del"), Ok(Delete));
        assert_eq!(key_code.parse("esc"), Ok(Escape));
        assert!(key_code.parse("∆").is_err());
    }
}
