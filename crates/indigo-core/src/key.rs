use flagset::{FlagSet, flags};
use std::{
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    str::FromStr,
};
use winnow::{
    ascii::{multispace0, till_line_ending},
    combinator::{alt, delimited, repeat, terminated},
    prelude::*,
    token::one_of,
};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Keys(pub Vec<Key>);

impl Deref for Keys {
    type Target = Vec<Key>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Keys {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for Keys {
    type Item = Key;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Keys {
    type Item = &'a Key;

    type IntoIter = std::slice::Iter<'a, Key>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

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

fn comment(input: &mut &str) -> ModalResult<()> {
    ('#', till_line_ending).void().parse_next(input)
}

fn ws_and_comments(input: &mut &str) -> ModalResult<()> {
    multispace0.parse_next(input)?;
    while comment.parse_next(input).is_ok() {
        multispace0.parse_next(input)?;
    }
    Ok(())
}

fn keys(input: &mut &str) -> ModalResult<Keys> {
    repeat(0.., delimited(ws_and_comments, key, ws_and_comments))
        .map(Keys)
        .parse_next(input)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Key {
    pub modifiers: KeyModifiers,
    pub code: KeyCode,
}

impl Key {
    pub fn normalize(&mut self) {
        if !self.modifiers.contains(KeyModifier::Shift) {
            return;
        }
        if let KeyCode::Char(c @ ('a'..='z' | 'A'..='Z')) = self.code {
            self.modifiers -= KeyModifier::Shift;
            self.code = KeyCode::Char(c.to_ascii_uppercase());
        }
    }
}

#[cfg(any(feature = "arbitrary", test))]
impl<'a> Arbitrary<'a> for Key {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut modifiers = FlagSet::default();
        for modifier in FlagSet::<KeyModifier>::full() {
            if u.arbitrary()? {
                modifiers |= modifier;
            }
        }
        let code = u.arbitrary()?;
        Ok(Self { modifiers, code })
    }
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
            KeyCode::Char(' ') => "space",
            KeyCode::Char('#') => "hash",
            KeyCode::Char('<') => "lt",
            KeyCode::Char('>') => "gt",
            KeyCode::Char(c) => &c.to_string(),
        };
        if self.modifiers.is_empty()
            && matches!(self.code, KeyCode::Char(_))
            && !matches!(self.code, KeyCode::Char(' ' | '#' | '<' | '>'))
        {
            write!(f, "{code}")?;
        } else {
            write!(f, "<")?;
            if self.modifiers.contains(KeyModifier::Control) {
                write!(f, "c-")?;
            }
            if self.modifiers.contains(KeyModifier::Alt) {
                write!(f, "a-")?;
            }
            if self.modifiers.contains(KeyModifier::Shift) {
                write!(f, "s-")?;
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
    let modifiers = repeat(0.., terminated(key_modifier, "-"))
        .fold(FlagSet::default, |x, y| x | y)
        .parse_next(input)?;
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
    #[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
    #[derive(Hash, Ord, PartialOrd)]
    pub enum KeyModifier: u8 {
        Control,
        Alt,
        Shift,
    }
}

pub type KeyModifiers = FlagSet<KeyModifier>;

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
    // NOTE: Replace `char` with `AsciiChar` once it stabilizes.
    // https://github.com/rust-lang/rust/issues/110998
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
        "space".value(KeyCode::Char(' ')),
        "hash".value(KeyCode::Char('#')),
        "lt".value(KeyCode::Char('<')),
        "gt".value(KeyCode::Char('>')),
    ))
    .parse_next(input)
}

fn key_code_bare(input: &mut &str) -> ModalResult<KeyCode> {
    one_of(' '..='~')
        .verify(|c| *c != ' ' && *c != '#')
        .map(KeyCode::Char)
        .parse_next(input)
}

#[cfg(any(feature = "arbitrary", test))]
impl<'a> Arbitrary<'a> for KeyCode {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        if u.ratio(8, 10)? {
            loop {
                let i = u.int_in_range(b' '..=b'~')?;
                if i != b' ' && i != b'#' {
                    let c = char::from(i);
                    return Ok(Self::Char(c));
                }
            }
        } else {
            Ok(*u.choose(&[
                Self::Backspace,
                Self::Delete,
                Self::Return,
                Self::Left,
                Self::Right,
                Self::Up,
                Self::Down,
                Self::Tab,
                Self::Escape,
            ])?)
        }
    }
}

impl From<char> for KeyCode {
    fn from(c: char) -> Self {
        Self::Char(c)
    }
}

#[must_use]
pub fn is(x: &Key, y: &str) -> bool {
    let mut x = *x;
    let mut y = y.parse::<Key>().unwrap();
    x.normalize();
    y.normalize();
    x == y
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

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
        // TODO: Distinguish between shift+a and A? Or normalize both to the same thing?
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
    fn test_parse_key_roundtrip() {
        arbtest(|u| {
            let key = u.arbitrary::<Key>()?;
            match key.to_string().parse() {
                Ok(parsed_key) => assert_eq!(key, parsed_key),
                Err(e) => panic!("Failed to parse `{key:?}` printed as `{key}`:\n{e}"),
            }
            Ok(())
        });
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
        assert_eq!(key_code.parse("space"), Ok(Char(' ')));
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

    #[test]
    fn test_parse_space_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(key.parse("<space>"), Ok(Key::from(Char(' '))));
        assert_eq!(key.parse("<c-space>"), Ok(Key::from((Control, Char(' ')))));
        assert_eq!(key.parse("<a-space>"), Ok(Key::from((Alt, Char(' ')))));
        assert_eq!(key.parse("<s-space>"), Ok(Key::from((Shift, Char(' ')))));
        assert_eq!(
            key.parse("<c-a-space>"),
            Ok(Key::from(([Control, Alt], Char(' '))))
        );
    }

    #[test]
    fn test_whitespace_stripping() {
        use KeyCode::*;
        // Whitespace should be stripped between keys
        assert_eq!(
            keys.parse("a b c"),
            Ok(Keys(vec![
                Key::from(Char('a')),
                Key::from(Char('b')),
                Key::from(Char('c')),
            ]))
        );
        // Newlines and tabs should also be stripped
        assert_eq!(
            keys.parse("a\nb\tc"),
            Ok(Keys(vec![
                Key::from(Char('a')),
                Key::from(Char('b')),
                Key::from(Char('c')),
            ]))
        );
        // Multiple whitespace characters should be stripped
        assert_eq!(
            keys.parse("a  \n\t  b"),
            Ok(Keys(vec![Key::from(Char('a')), Key::from(Char('b')),]))
        );
        // Whitespace at the beginning and end should be stripped
        assert_eq!(
            keys.parse("  \n\t  a  \n\t  "),
            Ok(Keys(vec![Key::from(Char('a')),]))
        );
        // Space key must use <space> notation
        assert_eq!(
            keys.parse("a<space>b"),
            Ok(Keys(vec![
                Key::from(Char('a')),
                Key::from(Char(' ')),
                Key::from(Char('b')),
            ]))
        );
    }

    #[test]
    fn test_print_space_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(Key::from(Char(' ')).to_string(), "<space>");
        assert_eq!(Key::from((Control, Char(' '))).to_string(), "<c-space>");
        assert_eq!(Key::from((Alt, Char(' '))).to_string(), "<a-space>");
        assert_eq!(
            Key::from(([Control, Alt], Char(' '))).to_string(),
            "<c-a-space>"
        );
    }

    #[test]
    fn test_parse_hash_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(key.parse("<hash>"), Ok(Key::from(Char('#'))));
        assert_eq!(key.parse("<c-hash>"), Ok(Key::from((Control, Char('#')))));
        assert_eq!(key.parse("<a-hash>"), Ok(Key::from((Alt, Char('#')))));
        assert_eq!(key.parse("<s-hash>"), Ok(Key::from((Shift, Char('#')))));
        assert_eq!(
            key.parse("<c-a-hash>"),
            Ok(Key::from(([Control, Alt], Char('#'))))
        );
    }

    #[test]
    fn test_print_hash_key() {
        use KeyCode::*;
        use KeyModifier::*;
        assert_eq!(Key::from(Char('#')).to_string(), "<hash>");
        assert_eq!(Key::from((Control, Char('#'))).to_string(), "<c-hash>");
        assert_eq!(Key::from((Alt, Char('#'))).to_string(), "<a-hash>");
        assert_eq!(
            Key::from(([Control, Alt], Char('#'))).to_string(),
            "<c-a-hash>"
        );
    }

    #[test]
    fn test_comment_parsing() {
        use KeyCode::*;
        use KeyModifier::*;
        // Comment at start of line
        assert_eq!(
            keys.parse("# comment\na"),
            Ok(Keys(vec![Key::from(Char('a')),]))
        );
        // Indented comment
        assert_eq!(
            keys.parse("  # indented comment\na"),
            Ok(Keys(vec![Key::from(Char('a')),]))
        );
        // Comment after keys
        assert_eq!(
            keys.parse("a b # comment\nc"),
            Ok(Keys(vec![
                Key::from(Char('a')),
                Key::from(Char('b')),
                Key::from(Char('c')),
            ]))
        );
        // Multiple comments
        assert_eq!(
            keys.parse("# comment 1\n# comment 2\na"),
            Ok(Keys(vec![Key::from(Char('a')),]))
        );
        // Comment with keys before and after
        assert_eq!(
            keys.parse("a\n# comment\nb"),
            Ok(Keys(vec![Key::from(Char('a')), Key::from(Char('b')),]))
        );
        // Comment without trailing newline
        assert_eq!(
            keys.parse("a # comment"),
            Ok(Keys(vec![Key::from(Char('a')),]))
        );
        // Empty lines with comments
        assert_eq!(
            keys.parse("a\n\n# comment\n\nb"),
            Ok(Keys(vec![Key::from(Char('a')), Key::from(Char('b')),]))
        );
        // Complex example from requirements
        assert_eq!(
            keys.parse(
                "# comment at start\n  # indented comment\n  <a-a> b c d # comment following keys"
            ),
            Ok(Keys(vec![
                Key::from(([Alt], 'a')),
                Key::from(Char('b')),
                Key::from(Char('c')),
                Key::from(Char('d')),
            ]))
        );
    }
}
