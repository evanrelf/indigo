use illuso_tui::*;
use std::io::{self, Write as _};

fn main() -> io::Result<()> {
    enable_raw_mode()?;

    let mut parser = Parser::new();

    if !enable_in_band_resize(&mut parser)? {
        disable_raw_mode()?;
        panic!("failed to enable in-band resize (mode 2048)");
    }

    let mut stdout = io::stdout().lock();
    let pid = std::process::id();

    loop {
        if let Some(Event::Resize { height, width, .. }) = read_event(&mut parser, None)? {
            write!(
                stdout,
                "{}{}pid:{pid},height:{height},width:{width}",
                MoveToColumn(1),
                CLEAR_LINE,
            )?;
            stdout.flush()?;
        }
    }
}
