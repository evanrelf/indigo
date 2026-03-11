use illuso_term::{Tty, output};
use std::{
    io::{self, Write as _},
    thread,
    time::Duration,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;

    write!(&mut tty, "{}", output::HIDE_CURSOR)?;
    tty.flush()?;

    let n = 3;

    for i in 0..n {
        write!(
            &mut tty,
            "{}{}",
            output::CLEAR_LINE,
            output::CursorColumn(1)
        )?;
        write!(&mut tty, "{}", n - i)?;
        tty.flush()?;
        thread::sleep(Duration::from_secs(1));
    }

    write!(
        &mut tty,
        "{}{}{}",
        output::CLEAR_LINE,
        output::CursorColumn(1),
        output::SHOW_CURSOR,
    )?;
    tty.flush()?;

    Ok(())
}
