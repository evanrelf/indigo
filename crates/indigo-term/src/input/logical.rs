use crate::{
    input::{ColorScheme, Event, Key, KeyCode, KeyModifiers, ModeSetting, physical::Token},
    output::KeyboardEnhancementFlags,
};
use std::str;
use winnow::{
    ascii::digit1,
    combinator::{alt, opt, separated},
    prelude::*,
    token::one_of,
};

pub fn decode(token: Token) -> Event {
    let Token::Csi {
        parameter_bytes,
        intermediate_bytes,
        final_byte,
    } = &token
    else {
        return Event::Unknown(token);
    };
    let bytes = [
        b"\x1b[",
        &parameter_bytes[..],
        &intermediate_bytes[..],
        &[*final_byte],
    ]
    .concat();
    alt((
        kitty_keyboard_flags,
        in_band_resize,
        da1,
        decrpm,
        color_scheme,
        kitty_key_u,
        kitty_key_letter,
    ))
    .parse(&bytes[..])
    .unwrap_or(Event::Unknown(token))
}

fn kitty_keyboard_flags(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    let flags = u8
        .verify_map(|n| KeyboardEnhancementFlags::from_bits(n))
        .parse_next(input)?;
    "u".void().parse_next(input)?;
    Ok(Event::KeyboardEnhancementFlags(flags))
}

fn kitty_key_u(input: &mut &[u8]) -> winnow::ModalResult<Event> {
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

fn kitty_key_letter(input: &mut &[u8]) -> winnow::ModalResult<Event> {
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

fn modifiers_and_event_type(input: &mut &[u8]) -> winnow::ModalResult<(KeyModifiers, u8)> {
    let mods_raw = u8.parse_next(input)?;
    let modifiers = KeyModifiers::from_bits_truncate(mods_raw.saturating_sub(1));
    let event_type = opt((":".void(), u8))
        .map(|o| o.map_or(1, |((), t)| t))
        .parse_next(input)?;
    Ok((modifiers, event_type))
}

fn resolve_shifted_char(key: &mut Key) {
    if key.modifiers == KeyModifiers::SHIFT
        && let KeyCode::Char(c) = key.code
        && let Some(shifted) = shifted_char(c)
    {
        key.code = KeyCode::Char(shifted);
        key.modifiers = KeyModifiers::empty();
    }
}

fn shifted_char(c: char) -> Option<char> {
    match c {
        'a'..='z' => Some(c.to_ascii_uppercase()),
        '1' => Some('!'),
        '2' => Some('@'),
        '3' => Some('#'),
        '4' => Some('$'),
        '5' => Some('%'),
        '6' => Some('^'),
        '7' => Some('&'),
        '8' => Some('*'),
        '9' => Some('('),
        '0' => Some(')'),
        '-' => Some('_'),
        '=' => Some('+'),
        '[' => Some('{'),
        ']' => Some('}'),
        '\\' => Some('|'),
        ';' => Some(':'),
        '\'' => Some('"'),
        ',' => Some('<'),
        '.' => Some('>'),
        '/' => Some('?'),
        '`' => Some('~'),
        _ => None,
    }
}

fn make_key_event(mut key: Key, event_type: u8) -> Event {
    resolve_shifted_char(&mut key);
    match event_type {
        2 => Event::KeyRepeat(key),
        3 => Event::KeyRelease(key),
        _ => Event::KeyPress(key),
    }
}

fn u32(input: &mut &[u8]) -> winnow::ModalResult<u32> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

fn in_band_resize(input: &mut &[u8]) -> winnow::ModalResult<Event> {
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

fn da1(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    separated(0.., digit1, ";").map(|()| ()).parse_next(input)?;
    "c".void().parse_next(input)?;
    Ok(Event::Da1)
}

fn decrpm(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    ("\x1b[", opt("?")).void().parse_next(input)?;
    let mode = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let setting = u8
        .verify_map(|n| match n {
            0 => Some(ModeSetting::NotRecognized),
            1 => Some(ModeSetting::Set),
            2 => Some(ModeSetting::Reset),
            3 => Some(ModeSetting::PermanentlySet),
            4 => Some(ModeSetting::PermanentlyReset),
            _ => None,
        })
        .parse_next(input)?;
    "$y".void().parse_next(input)?;
    Ok(Event::Decrpm { mode, setting })
}

fn color_scheme(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    "\x1b[?997;".void().parse_next(input)?;
    let color_scheme = u8
        .verify_map(|n| match n {
            1 => Some(ColorScheme::Dark),
            2 => Some(ColorScheme::Light),
            _ => None,
        })
        .parse_next(input)?;
    "n".void().parse_next(input)?;
    Ok(Event::ColorScheme(color_scheme))
}

fn u8(input: &mut &[u8]) -> winnow::ModalResult<u8> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

fn u16(input: &mut &[u8]) -> winnow::ModalResult<u16> {
    digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::Decoder;

    fn decode_bytes(input: &[u8]) -> Option<Event> {
        let mut decoder = Decoder::new();
        assert_eq!(decoder.feed(input), input.len());
        decoder.next_event()
    }

    #[test]
    fn parses_kitty_keyboard_flags() {
        let input = b"\x1b[?3u";
        let output = Event::KeyboardEnhancementFlags(
            KeyboardEnhancementFlags::DISAMBIGUATE | KeyboardEnhancementFlags::REPORT_EVENTS,
        );
        assert_eq!(
            kitty_keyboard_flags.parse(&input[..]).as_ref().ok(),
            Some(&output)
        );
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_in_band_resize() {
        let input = b"\x1b[48;40;80;0;0t";
        let output = Event::Resize {
            height: 40,
            width: 80,
        };
        assert_eq!(
            in_band_resize.parse(&input[..]).as_ref().ok(),
            Some(&output)
        );
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_da1() {
        let input = b"\x1b[?64;1234c";
        let output = Event::Da1;
        assert_eq!(da1.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_ansi_decrpm() {
        let input = b"\x1b[1234;1$y";
        let output = Event::Decrpm {
            mode: 1234,
            setting: ModeSetting::Set,
        };
        assert_eq!(decrpm.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_dec_decrpm() {
        let input = b"\x1b[?1234;2$y";
        let output = Event::Decrpm {
            mode: 1234,
            setting: ModeSetting::Reset,
        };
        assert_eq!(decrpm.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_color_scheme_dark() {
        let input = b"\x1b[?997;1n";
        let output = Event::ColorScheme(ColorScheme::Dark);
        assert_eq!(color_scheme.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_color_scheme_light() {
        let input = b"\x1b[?997;2n";
        let output = Event::ColorScheme(ColorScheme::Light);
        assert_eq!(color_scheme.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_plain() {
        let input = b"\x1b[97u";
        let output = Event::KeyPress(Key {
            code: KeyCode::Char('a'),
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_with_modifiers() {
        let input = b"\x1b[97;6u";
        let output = Event::KeyPress(Key {
            code: KeyCode::Char('a'),
            modifiers: KeyModifiers::CTRL | KeyModifiers::SHIFT,
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_release() {
        let input = b"\x1b[97;1:3u";
        let output = Event::KeyRelease(Key {
            code: KeyCode::Char('a'),
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_repeat() {
        let input = b"\x1b[97;1:2u";
        let output = Event::KeyRepeat(Key {
            code: KeyCode::Char('a'),
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_esc() {
        let input = b"\x1b[27u";
        let output = Event::KeyPress(Key {
            code: KeyCode::Esc,
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_key_enter() {
        let input = b"\x1b[13u";
        let output = Event::KeyPress(Key {
            code: KeyCode::Enter,
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(kitty_key_u.parse(&input[..]).as_ref().ok(), Some(&output));
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_arrow_no_modifiers() {
        let input = b"\x1b[A";
        let output = Event::KeyPress(Key {
            code: KeyCode::Up,
            modifiers: KeyModifiers::empty(),
        });
        assert_eq!(
            kitty_key_letter.parse(&input[..]).as_ref().ok(),
            Some(&output)
        );
        assert_eq!(decode_bytes(input), Some(output));
    }

    #[test]
    fn parses_kitty_arrow_with_modifiers() {
        let input = b"\x1b[1;2A";
        let output = Event::KeyPress(Key {
            code: KeyCode::Up,
            modifiers: KeyModifiers::SHIFT,
        });
        assert_eq!(
            kitty_key_letter.parse(&input[..]).as_ref().ok(),
            Some(&output)
        );
        assert_eq!(decode_bytes(input), Some(output));
    }
}
