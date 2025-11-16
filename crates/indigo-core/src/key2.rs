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
    combinator::{alt, cut_err, delimited, fail, opt, permutation, preceded, repeat, terminated},
    prelude::*,
    token::one_of,
};

pub struct Keys(pub Vec<Key>);

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
    repeat(0.., delimited(not_key, key, not_key))
        .map(Keys)
        .parse_next(input)
}

pub struct Key {
    pub modifiers: KeyModifiers,
    pub code: KeyCode,
}

fn key(input: &mut &str) -> ModalResult<Key> {
    alt((key_wrapped, key_bare)).parse_next(input)
}

fn key_wrapped(input: &mut &str) -> ModalResult<Key> {
    let _ = "<".parse_next(input)?;
    let (control, alt, shift) = permutation((
        opt("c-".value(KeyModifier::Control)),
        opt("a-".value(KeyModifier::Alt)),
        opt("s-".value(KeyModifier::Shift)),
    ))
    .parse_next(input)?;
    let modifiers = [control, alt, shift]
        .into_iter()
        .flatten()
        .fold(KeyModifiers::default(), |x, y| x | y);
    let code = key_code_wrapped.parse_next(input)?;
    let _ = ">".parse_next(input)?;
    Ok(Key { modifiers, code })
}

fn key_bare(input: &mut &str) -> ModalResult<Key> {
    alt((key_bare_modified, key_bare_unmodified)).parse_next(input)
}

fn key_bare_modified(input: &mut &str) -> ModalResult<Key> {
    let _ = "<".parse_next(input)?;
    let (control, alt, shift) = permutation((
        opt("c-".value(KeyModifier::Control)),
        opt("a-".value(KeyModifier::Alt)),
        opt("s-".value(KeyModifier::Shift)),
    ))
    .parse_next(input)?;
    let modifiers = [control, alt, shift]
        .into_iter()
        .flatten()
        .fold(KeyModifiers::default(), |x, y| x | y);
    if modifiers.is_empty() {
        fail.parse_next(input)?;
    }
    let code = key_code_bare.parse_next(input)?;
    let _ = ">".parse_next(input)?;
    Ok(Key { modifiers, code })
}

fn key_bare_unmodified(input: &mut &str) -> ModalResult<Key> {
    let modifiers = FlagSet::default();
    let code = key_code_bare.parse_next(input)?;
    Ok(Key { modifiers, code })
}

pub type KeyModifiers = FlagSet<KeyModifier>;

flags! {
    pub enum KeyModifier: u8 {
        Control,
        Alt,
        Shift,
    }
}

#[derive(Clone, Copy)]
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

    // #[test]
    // fn test_parse_key_roundtrip() {
    //     arbtest(|u| {
    //         let key = u.arbitrary::<Key>()?;
    //         match key.to_string().parse() {
    //             Ok(parsed_key) => assert_eq!(key, parsed_key),
    //             Err(e) => panic!("Failed to parse `{key:?}` printed as `{key}`:\n{e}"),
    //         }
    //         Ok(())
    //     });
    // }
}
