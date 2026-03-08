use illuso_term::*;
use std::{
    io::{self, Write as _},
    process,
};

fn main() -> io::Result<()> {
    enable_raw_mode()?;

    let mut parser = Parser::new();

    if !enable_in_band_resize(&mut parser)? {
        disable_raw_mode()?;
        panic!("failed to enable in-band resize (mode 2048)");
    }

    let mut stdout = io::stdout().lock();
    let pid = process::id();

    loop {
        if let Some(Event::Resize { height, width, .. }) = read_event(&mut parser, None)? {
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
