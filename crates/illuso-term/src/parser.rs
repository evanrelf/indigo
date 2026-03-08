use crate::event::Event;

pub fn parse(input: &mut &[u8]) -> Option<Event> {
    assert!(!input.is_empty());

    if input[0] == /* ESC */ 0x1b && input.len() > 1 {
        match input[1] {
            b'[' => parse_csi(input),
            _ => None,
        }
    } else {
        parse_ground(input)
    }
}

fn parse_ground(input: &mut &[u8]) -> Option<Event> {
    assert!(!input.is_empty());

    todo!()
}

fn parse_csi(input: &mut &[u8]) -> Option<Event> {
    assert_eq!(input[0..2], [0x1b, b'[']);

    // 'c' => {
    //     // Primary DA (CSI ? Pm c)
    //     std.debug.assert(sequence.len >= 4); // ESC [ ? c == 4 bytes
    //     switch (input[2]) {
    //         '?' => return .{ .event = .cap_da1, .n = sequence.len },
    //         else => return null_event,
    //     }
    // },

    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_da1() {
        // CSI ? 64 ; Ps1 ; ... Psn c
        let mut input: &[u8] = b"\x1b[?64;1234c";
        let output = Some(Event::Da1);
        assert_eq!(parse(&mut input), output);
        // TODO: Assert that input was consumed
    }

    #[test]
    fn parses_ansi_decrpm() {
        // CSI Pa ; Ps $ y
        let mut input: &[u8] = b"\x1b[1234;1$y";
        let output = Some(Event::Decrpm {
            mode: 1234,
            value: 1,
        });
        assert_eq!(parse(&mut input), output);
        // TODO: Assert that input was consumed
    }

    #[test]
    fn parses_dec_decrpm() {
        // CSI ? Pd ; Ps $ y
        let mut input: &[u8] = b"\x1b[?1234;1$y";
        let output = Some(Event::Decrpm {
            mode: 1234,
            value: 1,
        });
        assert_eq!(parse(&mut input), output);
        // TODO: Assert that input was consumed
    }
}
