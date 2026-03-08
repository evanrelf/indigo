use crate::event::Event;
use std::str;
use winnow::{
    ascii::digit1,
    combinator::{alt, cut_err, opt, separated},
    prelude::*,
};

// CSI ? Ps1 ; ... Psn c
fn da1(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    cut_err("\x1b[?").void().parse_next(input)?;
    separated(0.., digit1, ";").map(|()| ()).parse_next(input)?;
    "c".void().parse_next(input)?;
    Ok(Event::Da1)
}

// CSI Pa ; Ps $ y
// CSI ? Pd ; Ps $ y
fn decrpm(input: &mut &[u8]) -> winnow::ModalResult<Event> {
    cut_err(("\x1b[", opt("?"))).void().parse_next(input)?;
    let mode = digit1
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)?;
    ";".void().parse_next(input)?;
    let value = alt((b"0", b"1", b"2", b"3", b"4"))
        .try_map(|bytes| str::from_utf8(bytes))
        .try_map(|str| str::parse(str))
        .parse_next(input)?;
    "$y".void().parse_next(input)?;
    Ok(Event::Decrpm { mode, value })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_da1() {
        let input = b"\x1b[?64;1234c";
        let output = Ok((&b""[..], Event::Da1));
        assert_eq!(da1.parse_peek(input), output);
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
    }
}
