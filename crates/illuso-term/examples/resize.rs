use illuso_term::{escape, event::Event, reader::Reader, tty::Tty};
use std::{
    io::{self, Write as _},
    process,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

    write!(tty, "{}", escape::IN_BAND_RESIZE_QUERY)?;
    tty.flush()?;

    match reader.read_event(&mut tty)? {
        Event::Decrpm { mode: 2048, value } => match value {
            1 | 3 => {
                // Already set (1) or permanently set (3), no action needed
            }
            2 => {
                // Reset but settable, enable it
                write!(tty, "{}", escape::IN_BAND_RESIZE_SET)?;
                tty.flush()?;
            }
            _ => panic!("failed to enable in-band resize (mode 2048)"),
        },
        event => panic!("unexpected terminal input event: {event:?}"),
    }

    let mut stdout = io::stdout().lock();
    let pid = process::id();

    loop {
        if let Event::Resize { height, width } = reader.read_event(&mut tty)? {
            write!(
                stdout,
                "{}{}pid:{pid},height:{height},width:{width}",
                escape::MoveToColumn(1),
                escape::CLEAR_LINE,
            )?;
            stdout.flush()?;
        }
    }
}
