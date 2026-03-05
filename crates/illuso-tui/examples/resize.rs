use illuso_tui::{
    event::{self, Event},
    terminal,
};
use std::io::{self, Write as _};

fn main() -> io::Result<()> {
    terminal::enable_raw_mode()?;

    if !event::supports_in_band_resize()? {
        terminal::disable_raw_mode()?;
        panic!("terminal does not support in-band resize (mode 2048)");
    }

    let mut stdout = io::stdout().lock();
    write!(
        stdout,
        "{}{}",
        terminal::HIDE_CURSOR,
        terminal::ENABLE_IN_BAND_RESIZE
    )?;
    stdout.flush()?;

    let pid = std::process::id();

    loop {
        match event::read_event(None)? {
            Some(Event::Resize { height, width, .. }) => {
                write!(
                    stdout,
                    "{}{}pid:{pid},height:{height},width:{width}",
                    terminal::MoveToColumn(1),
                    terminal::CLEAR_LINE,
                )?;
                stdout.flush()?;
            }
            None => {}
        }
    }
}
