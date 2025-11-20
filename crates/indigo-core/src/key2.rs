#![expect(unused)] // TODO

use flagset::{FlagSet, flags};
use std::{
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    str::FromStr,
};
use winnow::{
    ascii::{multispace0, till_line_ending},
    combinator::{alt, cut_err, delimited, fail, opt, preceded, repeat, terminated},
    error::{StrContext, StrContextValue},
    prelude::*,
    token::one_of,
};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Clone, Debug)]
pub struct Keys(pub Vec<Key>);

impl FromStr for Keys {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        keys.parse(s).map_err(|e| anyhow::format_err!("{e}"))
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

fn comment(input: &mut &str) -> ModalResult<()> {
    ('#', till_line_ending).void().parse_next(input)
}

fn not_key(input: &mut &str) -> ModalResult<()> {
    multispace0.parse_next(input)?;
    while comment.parse_next(input).is_ok() {
        multispace0.parse_next(input)?;
    }
    Ok(())
}

fn keys(input: &mut &str) -> ModalResult<Keys> {
    not_key.parse_next(input)?;
    repeat(0.., terminated(key, not_key))
        .map(Keys)
        .parse_next(input)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Key {
    pub modifiers: KeyModifiers,
    pub code: KeyCode,
}

#[cfg(any(feature = "arbitrary", test))]
impl<'a> Arbitrary<'a> for Key {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut modifiers = KeyModifiers::default();
        for modifier in FlagSet::<KeyModifier>::full() {
            if u.arbitrary()? {
                *modifiers |= modifier;
            }
        }
        let code = u.arbitrary()?;
        Ok(Self { modifiers, code })
    }
}

impl FromStr for Key {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| anyhow::format_err!("{e}"))
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
            KeyCode::Char(' ') => "\\ ",
            KeyCode::Char('#') => "\\#",
            KeyCode::Char('<') => "\\<",
            KeyCode::Char('>') => "\\>",
            KeyCode::Char('\\') => "\\\\",
            KeyCode::Char('\t') => "\\t",
            KeyCode::Char('\n') => "\\n",
            KeyCode::Char('\r') => "\\r",
            KeyCode::Char(c) => &c.to_string(),
        };
        if self.modifiers.is_empty()
            && let KeyCode::Char(_) = self.code
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

fn key(input: &mut &str) -> ModalResult<Key> {
    alt((key_wrapped, key_bare_unmodified)).parse_next(input)
}

fn key_wrapped(input: &mut &str) -> ModalResult<Key> {
    let k = |input: &mut &str| {
        let modifiers = key_modifiers
            .context(StrContext::Label("key modifiers"))
            .parse_next(input)?;
        let allow_bare = !modifiers.is_empty();
        let code = alt((key_code_wrapped, key_code_bare.verify(|_| allow_bare)))
            .context(StrContext::Label("key code"))
            .parse_next(input)?;
        Ok(Key { modifiers, code })
    };
    delimited("<", cut_err(k), ">").parse_next(input)
}

fn key_bare_unmodified(input: &mut &str) -> ModalResult<Key> {
    let modifiers = KeyModifiers::default();
    let code = key_code_bare.parse_next(input)?;
    Ok(Key { modifiers, code })
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct KeyModifiers(pub FlagSet<KeyModifier>);

impl FromStr for KeyModifiers {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_modifiers
            .parse(s)
            .map_err(|e| anyhow::format_err!("{e}"))
    }
}

fn key_modifiers(input: &mut &str) -> ModalResult<KeyModifiers> {
    let mut modifiers = KeyModifiers::default();
    while !modifiers.is_full() {
        let Some(modifier) = opt(terminated(key_modifier, "-")).parse_next(input)? else {
            break;
        };
        if modifiers.contains(modifier) {
            return fail.parse_next(input);
        }
        *modifiers |= modifier;
    }
    Ok(modifiers)
}

impl Deref for KeyModifiers {
    type Target = FlagSet<KeyModifier>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for KeyModifiers {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

flags! {
    #[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
    pub enum KeyModifier: u8 {
        Control,
        Alt,
        Shift,
    }
}

impl FromStr for KeyModifier {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_modifier
            .parse(s)
            .map_err(|e| anyhow::format_err!("{e}"))
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

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[cfg(any(feature = "arbitrary", test))]
impl<'a> Arbitrary<'a> for KeyCode {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        if u.ratio(8, 10)? {
            loop {
                let i = u.int_in_range(b' '..=b'~')?;
                let c = char::from(i);
                if !" #<>\\".contains(c) {
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

impl FromStr for KeyCode {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_code.parse(s).map_err(|e| anyhow::format_err!("{e}"))
    }
}

fn key_code(input: &mut &str) -> ModalResult<KeyCode> {
    alt((key_code_escaped, key_code_wrapped, key_code_bare)).parse_next(input)
}

fn key_code_escaped(input: &mut &str) -> ModalResult<KeyCode> {
    preceded(
        '\\',
        cut_err(alt((
            ' '.value(KeyCode::Char(' ')),
            '#'.value(KeyCode::Char('#')),
            '<'.value(KeyCode::Char('<')),
            '>'.value(KeyCode::Char('>')),
            '\\'.value(KeyCode::Char('\\')),
            't'.value(KeyCode::Char('\t')),
            'n'.value(KeyCode::Char('\n')),
            'r'.value(KeyCode::Char('\r')),
        ))),
    )
    .parse_next(input)
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
    ))
    .parse_next(input)
}

fn key_code_bare(input: &mut &str) -> ModalResult<KeyCode> {
    one_of(' '..='~')
        .verify(|c| !" #<>\\".contains(*c))
        .map(KeyCode::Char)
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

    #[test]
    fn random() {
        let input = "
            # comment
            # comment
            abc a b c # comment

            \t \n <s-\n> <c-->

            # comment
            <c-a-del> <a-a> <tab> <s-tab> <a-esc>
        ";
        let expected = "abcabc\t\n<s-\n><c--><c-a-del><a-a><tab><s-tab><a-esc>";
        let actual = input.parse::<Keys>().unwrap().to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn weird() {
        assert_eq!("<c-->".parse::<Key>().unwrap().to_string(), "<c-->");
        assert_eq!("<c-\\>>".parse::<Key>().unwrap().to_string(), "<c-\\>>>");
    }

    #[test]
    fn escaped() {
        assert_eq!("\\n".parse::<Key>().unwrap().to_string(), "\\n");
        assert_eq!("\\t".parse::<Key>().unwrap().to_string(), "\\t");
        assert_eq!("\\\\".parse::<Key>().unwrap().to_string(), "\\\\");
    }

    #[test]
    fn no_wrapped_bare() {
        assert_eq!("a".parse::<Key>().unwrap().to_string(), "a");
        assert!("<a>".parse::<Key>().is_err());
    }

    #[test]
    fn modifier_ordering() {
        assert_eq!("<c-a-s-a>".parse::<Key>().unwrap().to_string(), "<c-a-s-a>");
        assert_eq!("<s-c-a>".parse::<Key>().unwrap().to_string(), "<c-s-a>");
        assert_eq!("<s-c-a-a>".parse::<Key>().unwrap().to_string(), "<c-a-s-a>");
        assert_eq!("<a-s-c-a>".parse::<Key>().unwrap().to_string(), "<c-a-s-a>");
    }

    #[test]
    fn no_duplicate_modifiers() {
        assert!("<c-c-a>".parse::<Key>().is_err());
        assert!("<s-c-a-c-a>".parse::<Key>().is_err());
    }

    #[test]
    fn key_roundtrip() {
        arbtest(|u| {
            let key = u.arbitrary::<Key>()?;
            match key.to_string().parse::<Key>() {
                Ok(parsed_key) => assert_eq!(key, parsed_key),
                Err(e) => panic!("Failed to parse `{key:?}` printed as `{key}`:\n{e}"),
            }
            Ok(())
        });
    }
}
