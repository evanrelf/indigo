use illuso_tui::{event::*, *};
use std::io::{self, Write as _};

fn main() -> io::Result<()> {
    enable_raw_mode()?;

    if !supports_in_band_resize()? {
        disable_raw_mode()?;
        panic!("terminal does not support in-band resize (mode 2048)");
    }

    let mut stdout = io::stdout().lock();
    write!(stdout, "{HIDE_CURSOR}{ENABLE_IN_BAND_RESIZE}")?;
    stdout.flush()?;

    let pid = std::process::id();

    loop {
        match read_event(None)? {
            Some(Event::Resize { height, width, .. }) => {
                write!(
                    stdout,
                    "{}{}pid:{pid},height:{height},width:{width}",
                    MoveToColumn(1),
                    CLEAR_LINE,
                )?;
                stdout.flush()?;
            }
            None => {}
        }
    }
}
