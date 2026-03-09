use crate::event::Event;
use std::str;
use winnow::{
    ascii::digit1,
    combinator::{alt, cut_err, opt, peek, repeat, separated, seq},
    prelude::*,
    token::one_of,
};

fn event(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    csi.parse_next(input)
}

fn csi(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    peek("\x1b[").parse_next(input)?;
    cut_err(alt((in_band_resize, da1, decrpm, unknown_csi))).parse_next(input)
}

// CSI ? Ps1 ; ... Psn c
fn da1(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    "\x1b[?".void().parse_next(input)?;
    separated(0.., digit1, ";").map(|()| ()).parse_next(input)?;
    "c".void().parse_next(input)?;
    Ok(Event::Da1)
}

// CSI Pa ; Ps $ y
// CSI ? Pd ; Ps $ y
fn decrpm(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    ("\x1b[", opt("?")).void().parse_next(input)?;
    let mode = u16.parse_next(input)?;
    ";".void().parse_next(input)?;
    let value = u8.verify(|n| (0..=4).contains(n)).parse_next(input)?;
    "$y".void().parse_next(input)?;
    Ok(Event::Decrpm { mode, value })
}

fn unknown_csi(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    "\x1b[".void().parse_next(input)?;
    let _parameter_bytes: () = repeat(0.., one_of(0x30..=0x3F).void()).parse_next(input)?;
    let _intermediate_bytes: () = repeat(0.., one_of(0x20..=0x2F).void()).parse_next(input)?;
    let _final_byte: () = one_of(0x40..=0x7E).void().parse_next(input)?;
    Ok(Event::UnknownCsi)
}

// CSI 48 ; height ; width ; height_pixels ; width_pixels t
fn in_band_resize(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    seq!(Event::Resize {
        _: "\x1b[48;",
        height: u16,
        _: ";",
        width: u16,
        _: ";",
        _: u16,
        _: ";",
        _: u16,
        _: "t",
    })
    .parse_next(input)
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

    #[test]
    fn parses_in_band_resize() {
        let input = b"\x1b[48;40;80;0;0t";
        let output = Ok((
            &b""[..],
            Event::Resize {
                height: 40,
                width: 80,
            },
        ));
        assert_eq!(in_band_resize.parse_peek(input), output);
        assert_eq!(csi.parse_peek(input), output);
        assert_eq!(event.parse_peek(input), output);
    }

    #[test]
    fn parses_da1() {
        let input = b"\x1b[?64;1234c";
        let output = Ok((&b""[..], Event::Da1));
        assert_eq!(da1.parse_peek(input), output);
        assert_eq!(csi.parse_peek(input), output);
        assert_eq!(event.parse_peek(input), output);
    }

    #[test]
    fn parses_ansi_decrpm() {
        let input = b"\x1b[1234;1$y";
        let output = Ok((
            &b""[..],
            Event::Decrpm {
                mode: 1234,
                value: 1,
            },
        ));
        assert_eq!(decrpm.parse_peek(input), output);
        assert_eq!(csi.parse_peek(input), output);
        assert_eq!(event.parse_peek(input), output);
    }

    #[test]
    fn parses_dec_decrpm() {
        let input = b"\x1b[?1234;1$y";
        let output = Ok((
            &b""[..],
            Event::Decrpm {
                mode: 1234,
                value: 1,
            },
        ));
        assert_eq!(decrpm.parse_peek(input), output);
        assert_eq!(csi.parse_peek(input), output);
        assert_eq!(event.parse_peek(input), output);
    }
}
