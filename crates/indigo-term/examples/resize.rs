#![allow(clippy::uninlined_format_args)]
#![allow(clippy::wildcard_imports)]

use indigo_term::*;
use std::{
    io::{self, Write as _},
    process,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

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

    let mut stdout = io::stdout().lock();
    let pid = process::id();

    loop {
        if let Event::Resize { height, width } = reader.read_event(&mut tty)? {
            write!(
                stdout,
                "{}{}pid:{pid},height:{height},width:{width}",
                CursorColumn(1),
                CLEAR_LINE,
            )?;
            stdout.flush()?;
        }
    }
}
