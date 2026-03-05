use illuso_tui::{
    escape,
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
        escape::HIDE_CURSOR,
        escape::ENABLE_IN_BAND_RESIZE
    )?;
    stdout.flush()?;

    let pid = std::process::id();

    loop {
        match event::read_event(None)? {
            Some(Event::Resize { height, width, .. }) => {
                write!(
                    stdout,
                    "{}{}pid:{pid},height:{height},width:{width}",
                    escape::MoveToColumn(1),
                    escape::CLEAR_LINE,
                )?;
                stdout.flush()?;
            }
            None => {}
        }
    }
}
