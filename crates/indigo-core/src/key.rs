use bitflags::bitflags;
use std::{
    fmt::{self, Display, Formatter},
    hash::Hash,
    str::FromStr,
};
use winnow::{
    ascii::{multispace0, till_line_ending},
    combinator::{alt, cut_err, delimited, fail, opt, preceded, repeat, terminated},
    error::StrContext,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
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

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Key {
    pub modifiers: KeyModifiers,
    pub code: KeyCode,
}

impl Key {
    pub fn normalize(&mut self) {
        if !self.modifiers.contains(KeyModifiers::SHIFT) {
            return;
        }
        if let KeyCode::Char(c @ (b'a'..=b'z' | b'A'..=b'Z')) = self.code {
            self.modifiers.remove(KeyModifiers::SHIFT);
            self.code = KeyCode::Char(c.to_ascii_uppercase());
        }
    }
}

impl FromStr for Key {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key.parse(s).map_err(|e| anyhow::format_err!("{e}"))
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if self.modifiers.is_empty()
            && let KeyCode::Char(_) = self.code
        {
            write!(f, "{}", self.code)?;
        } else {
            write!(f, "<{}{}>", self.modifiers, self.code)?;
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
    let modifiers = KeyModifiers::empty();
    let code = key_code_bare.parse_next(input)?;
    Ok(Key { modifiers, code })
}

bitflags! {
    #[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct KeyModifiers: u8 {
        const CONTROL = 1 << 0;
        const ALT = 1 << 1;
        const SHIFT = 1 << 2;
    }
}

impl FromStr for KeyModifiers {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        key_modifiers
            .parse(s)
            .map_err(|e| anyhow::format_err!("{e}"))
    }
}

impl Display for KeyModifiers {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if self.contains(Self::CONTROL) {
            write!(f, "c-")?;
        }
        if self.contains(Self::ALT) {
            write!(f, "a-")?;
        }
        if self.contains(Self::SHIFT) {
            write!(f, "s-")?;
        }
        Ok(())
    }
}

fn key_modifiers(input: &mut &str) -> ModalResult<KeyModifiers> {
    let mut modifiers = KeyModifiers::empty();
    while !modifiers.is_all() {
        let Some(modifier) = opt(alt((
            "c-".value(KeyModifiers::CONTROL),
            "a-".value(KeyModifiers::ALT),
            "s-".value(KeyModifiers::SHIFT),
        )))
        .parse_next(input)?
        else {
            break;
        };
        if modifiers.contains(modifier) {
            return fail.parse_next(input);
        }
        modifiers.insert(modifier);
    }
    Ok(modifiers)
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
    // NOTE: Replace `u8` with `AsciiChar` once it stabilizes.
    // https://github.com/rust-lang/rust/issues/110998
    Char(u8),
}

#[cfg(any(feature = "arbitrary", test))]
impl<'a> Arbitrary<'a> for KeyCode {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        if u.ratio(8, 10)? {
            loop {
                let c = u.int_in_range(b' '..=b'~')?;
                if !b" #<>\\".contains(&c) {
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

impl Display for KeyCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Backspace => write!(f, "bs")?,
            Self::Delete => write!(f, "del")?,
            Self::Return => write!(f, "ret")?,
            Self::Left => write!(f, "left")?,
            Self::Right => write!(f, "right")?,
            Self::Up => write!(f, "up")?,
            Self::Down => write!(f, "down")?,
            Self::Tab => write!(f, "tab")?,
            Self::Escape => write!(f, "esc")?,
            Self::Char(b' ') => write!(f, "\\ ")?,
            Self::Char(b'#') => write!(f, "\\#")?,
            Self::Char(b'<') => write!(f, "\\<")?,
            Self::Char(b'>') => write!(f, "\\>")?,
            Self::Char(b'\\') => write!(f, "\\\\")?,
            Self::Char(b'\t') => write!(f, "\\t")?,
            Self::Char(b'\n') => write!(f, "\\n")?,
            Self::Char(b'\r') => write!(f, "\\r")?,
            Self::Char(c) => write!(f, "{}", char::from(*c))?,
        }
        Ok(())
    }
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
    ))
    .parse_next(input)
}

fn key_code_bare(input: &mut &str) -> ModalResult<KeyCode> {
    alt((
        one_of(b' '..=b'~')
            .verify(|c| !" #<>\\".contains(*c))
            .map(|c| KeyCode::Char(u8::try_from(c).expect("Only parsing ASCII"))),
        key_code_escaped,
    ))
    .parse_next(input)
}

fn key_code_escaped(input: &mut &str) -> ModalResult<KeyCode> {
    preceded(
        '\\',
        cut_err(alt((
            ' '.value(KeyCode::Char(b' ')),
            '#'.value(KeyCode::Char(b'#')),
            '<'.value(KeyCode::Char(b'<')),
            '>'.value(KeyCode::Char(b'>')),
            '\\'.value(KeyCode::Char(b'\\')),
            't'.value(KeyCode::Char(b'\t')),
            'n'.value(KeyCode::Char(b'\n')),
            'r'.value(KeyCode::Char(b'\r')),
        ))),
    )
    .parse_next(input)
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
    fn random() {
        let input = "
            # comment
            # comment
            abc a b c # comment

            \\t \\n <s-\\n> <c-->

            # comment
            <c-a-del> <a-a> <tab> <s-tab> <a-esc>
        ";
        let expected = "abcabc\\t\\n<s-\\n><c--><c-a-del><a-a><tab><s-tab><a-esc>";
        let actual = input.parse::<Keys>().unwrap().to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn weird() {
        assert_eq!("<c-->".parse::<Key>().unwrap().to_string(), "<c-->");
        assert_eq!("<c-\\>>".parse::<Key>().unwrap().to_string(), "<c-\\>>");
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
