use illuso_term::*;
use std::{
    io::{self, Write as _},
    thread,
    time::Duration,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;

    write!(&mut tty, "{}", CURSOR_HIDE)?;
    tty.flush()?;

    let n = 3;

    for i in 0..n {
        write!(&mut tty, "{}{}", CLEAR_LINE, CursorColumn(1))?;
        write!(&mut tty, "{}", n - i)?;
        tty.flush()?;
        thread::sleep(Duration::from_secs(1));
    }

    write!(&mut tty, "{}{}{}", CLEAR_LINE, CursorColumn(1), CURSOR_SHOW,)?;
    tty.flush()?;

    Ok(())
}
