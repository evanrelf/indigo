#![allow(clippy::uninlined_format_args)]
#![allow(clippy::wildcard_imports)]

use indigo_term::{
    Reader, Tty,
    input::*,
    output::{KeyboardEnhancementFlags as KEF, *},
};
use std::{
    io::{self, Write as _},
    process,
};

const CTRL_C: Event = Event::KeyPress(Key {
    code: KeyCode::Char('c'),
    modifiers: KeyModifiers::CTRL,
});

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

    tty.enable_raw_mode()?;

    // Send the DECRQM query followed by DA1 as a sentinel
    write!(tty, "{}{}", IN_BAND_RESIZE_QUERY, DA1_QUERY)?;
    tty.flush()?;

    // Read events until we see either a DECRPM reply or DA1
    loop {
        match reader.read_event(&mut tty)? {
            Event::Decrpm {
                mode: 2048,
                setting,
            } => match setting {
                ModeSetting::Set | ModeSetting::PermanentlySet => break,
                ModeSetting::Reset => {
                    write!(tty, "{}", IN_BAND_RESIZE_SET)?;
                    tty.flush()?;
                    break;
                }
                ModeSetting::PermanentlyReset => {
                    drop(tty);
                    panic!("terminal does not support enabling in-band resize (mode 2048)");
                }
                ModeSetting::NotRecognized => {
                    drop(tty);
                    panic!("terminal does not support in-band resize (mode 2048)");
                }
            },
            Event::Da1 => {
                drop(tty);
                panic!("terminal does not support in-band resize (mode 2048)");
            }
            _ => {}
        }
    }

    // Raw mode disables signals, so Ctrl-C has to arrive as a key event
    write!(tty, "{}", KeyboardEnhancementFlagsPush(KEF::DISAMBIGUATE))?;
    tty.flush()?;

    let pid = process::id();

    loop {
        let event = reader.read_event(&mut tty)?;
        if let Event::Resize { height, width } = event {
            write!(
                tty,
                "{}{}pid:{pid},height:{height},width:{width}",
                CursorColumn(1),
                CLEAR_LINE,
            )?;
            tty.flush()?;
        } else if event == CTRL_C {
            break;
        }
    }

    write!(tty, "{}", IN_BAND_RESIZE_RESET)?;
    write!(tty, "{}", KeyboardEnhancementFlagsPop)?;
    write!(tty, "\n\r")?;
    tty.flush()?;

    Ok(())
}
