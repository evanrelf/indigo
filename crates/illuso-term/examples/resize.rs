use illuso_term::{Event, Reader, Tty, output};
use std::{
    io::{self, Write as _},
    process,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

    // Send the DECRQM query followed by DA1 as a sentinel
    write!(tty, "{}{}", output::IN_BAND_RESIZE_QUERY, output::DA1_QUERY)?;
    tty.flush()?;

    // Read events until we see either a DECRPM reply or DA1
    loop {
        match reader.read_event(&mut tty)? {
            Event::Decrpm { mode: 2048, value } => match value {
                1 | 3 => {
                    // Already set (1) or permanently set (3), no action needed
                    break;
                }
                2 => {
                    // Reset but settable, enable it
                    write!(tty, "{}", output::IN_BAND_RESIZE_SET)?;
                    tty.flush()?;
                    break;
                }
                4 => {
                    drop(tty);
                    panic!("terminal does not support enabling in-band resize (mode 2048)");
                }
                0 => {
                    drop(tty);
                    panic!("terminal does not support in-band resize (mode 2048)");
                }
                _ => {
                    drop(tty);
                    panic!("unexpected setting for in-band resize (mode 2048): {value}");
                }
            },
            Event::Da1 => {
                drop(tty);
                panic!("terminal does not recognize in-band resize (mode 2048)");
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
                output::CursorColumn(1),
                output::CLEAR_LINE,
            )?;
            stdout.flush()?;
        }
    }
}
