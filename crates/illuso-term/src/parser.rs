use crate::{
    escape::KittyKeyboardFlags,
    event::{Event, Key, KeyCode, KeyModifiers},
};
use std::{ops::Deref, str};
use tinyvec::TinyVec;
use winnow::{
    Partial,
    ascii::digit1,
    combinator::{alt, cut_err, opt, peek, repeat, separated},
    prelude::*,
    stream::Accumulate,
    token::one_of,
};

pub fn event(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    alt((csi, osc)).parse_next(input)
}

fn csi(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    peek("\x1b[").parse_next(input)?;
    cut_err(alt((
        kitty_keyboard_flags,
        in_band_resize,
        da1,
        decrpm,
        kitty_key_u,
        kitty_key_letter,
        unknown_csi,
    )))
    .parse_next(input)
}

fn kitty_keyboard_flags(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    let flags = u8
        .verify_map(|n| KittyKeyboardFlags::from_bits(n))
        .parse_next(input)?;
    "u".void().parse_next(input)?;
    Ok(Event::KittyKeyboardFlags(flags))
}

fn kitty_key_u(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[".void().parse_next(input)?;
    let key_code = u32.verify_map(key_code_from_number).parse_next(input)?;
    let (modifiers, event_type) = opt((";".void(), modifiers_and_event_type))
        .map(|o| o.map_or((KeyModifiers::empty(), 1), |((), m)| m))
        .parse_next(input)?;
    // Skip optional text-as-codepoints field
    opt((";".void(), separated(0.., digit1, ":").map(|()| ()))).parse_next(input)?;
    "u".void().parse_next(input)?;
    let key = Key {
        code: key_code,
        modifiers,
    };
    Ok(make_key_event(key, event_type))
}

fn key_code_from_number(n: u32) -> Option<KeyCode> {
    match n {
        9 => Some(KeyCode::Tab),
        13 => Some(KeyCode::Enter),
        27 => Some(KeyCode::Esc),
        127 => Some(KeyCode::Backspace),
        // Ignoring functional keys in the Unicode Private Use Area (F13+, keypad, media, standalone
        // modifiers, etc) for now.
        57344..=57454 => None,
        n => char::from_u32(n)
            .filter(|c| !c.is_control())
            .map(KeyCode::Char),
    }
}

fn kitty_key_letter(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[".void().parse_next(input)?;
    let (modifiers, event_type) = opt(("1;".void(), modifiers_and_event_type))
        .map(|o| o.map_or((KeyModifiers::empty(), 1), |((), m)| m))
        .parse_next(input)?;
    let key_code = one_of(b"ABCD")
        .verify_map(|b| match b {
            b'A' => Some(KeyCode::Up),
            b'B' => Some(KeyCode::Down),
            b'C' => Some(KeyCode::Right),
            b'D' => Some(KeyCode::Left),
            _ => None,
        })
        .parse_next(input)?;
    let key = Key {
        code: key_code,
        modifiers,
    };
    Ok(make_key_event(key, event_type))
}

fn modifiers_and_event_type(input: &mut Partial<&[u8]>) -> winnow::ModalResult<(KeyModifiers, u8)> {
    let mods_raw = u8.parse_next(input)?;
    let modifiers = KeyModifiers::from_bits_truncate(mods_raw.saturating_sub(1));
    let event_type = opt((":".void(), u8))
        .map(|o| o.map_or(1, |((), t)| t))
        .parse_next(input)?;
    Ok((modifiers, event_type))
}

fn make_key_event(key: Key, event_type: u8) -> Event {
    match event_type {
        2 => Event::KeyRepeat(key),
        3 => Event::KeyRelease(key),
        _ => Event::KeyPress(key),
    }
}

fn u32(input: &mut Partial<&[u8]>) -> winnow::ModalResult<u32> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

fn in_band_resize(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[48;".void().parse_next(input)?;
    let height = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let width = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let _height_pixels = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let _width_pixels = u16.parse_next(input)?;
    "t".void().parse_next(input)?;
    Ok(Event::Resize { height, width })
}

fn da1(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    separated(0.., digit1, ";").map(|()| ()).parse_next(input)?;
    "c".void().parse_next(input)?;
    Ok(Event::Da1)
}

fn decrpm(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    ("\x1b[", opt("?")).void().parse_next(input)?;
    let mode = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let value = u8.verify(|n| (0..=4).contains(n)).parse_next(input)?;
    "$y".void().parse_next(input)?;
    Ok(Event::Decrpm { mode, value })
}

fn unknown_csi(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[".void().parse_next(input)?;
    let MyTinyVec(parameter_bytes) = repeat(0.., one_of(0x30..=0x3F)).parse_next(input)?;
    if parameter_bytes.is_heap() {
        tracing::debug!(
            "got {} csi parameter bytes, overflowing to heap",
            parameter_bytes.len()
        );
    }
    let MyTinyVec(intermediate_bytes) = repeat(0.., one_of(0x20..=0x2F)).parse_next(input)?;
    if intermediate_bytes.is_heap() {
        tracing::debug!(
            "got {} csi intermediate bytes, overflowing to heap",
            intermediate_bytes.len()
        );
    }
    let final_byte = one_of(0x40..=0x7E).parse_next(input)?;
    Ok(Event::UnknownCsi {
        parameter_bytes,
        intermediate_bytes,
        final_byte,
    })
}

fn osc(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    peek("\x1b]").parse_next(input)?;
    cut_err(unknown_osc).parse_next(input)
}

fn unknown_osc(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b]".void().parse_next(input)?;
    let MyTinyVec(data_bytes) = repeat(0.., one_of(0x20..=0xFF)).parse_next(input)?;
    if data_bytes.is_heap() {
        tracing::debug!(
            "found {} osc intermediate bytes, overflowing to heap",
            data_bytes.len()
        );
    }
    alt(("\x07", "\x1b\x5c")).void().parse_next(input)?;
    Ok(Event::UnknownOsc { data_bytes })
}

fn u8(input: &mut Partial<&[u8]>) -> winnow::ModalResult<u8> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

fn u16(input: &mut Partial<&[u8]>) -> winnow::ModalResult<u16> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

struct MyTinyVec<T: tinyvec::Array>(TinyVec<T>);

impl<T: tinyvec::Array> Deref for MyTinyVec<T> {
    type Target = TinyVec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, const N: usize> Accumulate<T> for MyTinyVec<[T; N]>
where
    T: Default,
{
    fn initial(capacity: Option<usize>) -> Self {
        if let Some(capacity) = capacity {
            Self(TinyVec::with_capacity(capacity))
        } else {
            Self(TinyVec::new())
        }
    }
    fn accumulate(&mut self, acc: T) {
        self.0.push(acc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_kitty_keyboard_flags() {
        let input = b"\x1b[?3u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KittyKeyboardFlags(
                KittyKeyboardFlags::DISAMBIGUATE | KittyKeyboardFlags::REPORT_EVENTS,
            ),
        ));
        assert_eq!(kitty_keyboard_flags.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_in_band_resize() {
        let input = b"\x1b[48;40;80;0;0t";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::Resize {
                height: 40,
                width: 80,
            },
        ));
        assert_eq!(in_band_resize.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_da1() {
        let input = b"\x1b[?64;1234c";
        let output = Ok((Partial::new(&b""[..]), Event::Da1));
        assert_eq!(da1.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_ansi_decrpm() {
        let input = b"\x1b[1234;1$y";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::Decrpm {
                mode: 1234,
                value: 1,
            },
        ));
        assert_eq!(decrpm.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_dec_decrpm() {
        let input = b"\x1b[?1234;1$y";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::Decrpm {
                mode: 1234,
                value: 1,
            },
        ));
        assert_eq!(decrpm.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_plain() {
        let input = b"\x1b[97u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Char('a'),
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_with_modifiers() {
        let input = b"\x1b[97;6u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Char('a'),
                modifiers: KeyModifiers::CTRL | KeyModifiers::SHIFT,
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_release() {
        let input = b"\x1b[97;1:3u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyRelease(Key {
                code: KeyCode::Char('a'),
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_repeat() {
        let input = b"\x1b[97;1:2u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyRepeat(Key {
                code: KeyCode::Char('a'),
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_esc() {
        let input = b"\x1b[27u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Esc,
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_key_enter() {
        let input = b"\x1b[13u";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_u.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_arrow_no_modifiers() {
        let input = b"\x1b[A";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Up,
                modifiers: KeyModifiers::empty(),
            }),
        ));
        assert_eq!(kitty_key_letter.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }

    #[test]
    fn parses_kitty_arrow_with_modifiers() {
        let input = b"\x1b[1;2A";
        let output = Ok((
            Partial::new(&b""[..]),
            Event::KeyPress(Key {
                code: KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
            }),
        ));
        assert_eq!(kitty_key_letter.parse_peek(Partial::new(input)), output);
        assert_eq!(csi.parse_peek(Partial::new(input)), output);
        assert_eq!(event.parse_peek(Partial::new(input)), output);
    }
}
