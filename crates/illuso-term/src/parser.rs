use crate::{escape::KittyKeyboardFlags, event::Event};
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
    csi.parse_next(input)
}

fn csi(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    peek("\x1b[").parse_next(input)?;
    cut_err(alt((
        kitty_keyboard_flags,
        in_band_resize,
        da1,
        decrpm,
        unknown_csi,
    )))
    .parse_next(input)
}

// CSI ? flags u
fn kitty_keyboard_flags(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    let flags = u8
        .verify_map(|n| KittyKeyboardFlags::from_bits(n))
        .parse_next(input)?;
    "u".void().parse_next(input)?;
    Ok(Event::KittyKeyboardFlags(flags))
}

// CSI 48 ; height ; width ; height_pixels ; width_pixels t
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

// CSI ? Ps1 ; ... Psn c
fn da1(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    separated(0.., digit1, ";").map(|()| ()).parse_next(input)?;
    "c".void().parse_next(input)?;
    Ok(Event::Da1)
}

// CSI Pa ; Ps $ y
// CSI ? Pd ; Ps $ y
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
        tracing::warn!("found {} parameter bytes", parameter_bytes.len());
    }
    let MyTinyVec(intermediate_bytes) = repeat(0.., one_of(0x20..=0x2F)).parse_next(input)?;
    if intermediate_bytes.is_heap() {
        tracing::warn!("found {} intermediate bytes", intermediate_bytes.len());
    }
    let final_byte = one_of(0x40..=0x7E).parse_next(input)?;
    Ok(Event::UnknownCsi {
        parameter_bytes,
        intermediate_bytes,
        final_byte,
    })
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
}
