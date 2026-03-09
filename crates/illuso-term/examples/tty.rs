use illuso_term::{Tty, escape};
use std::{
    io::{self, Write as _},
    thread,
    time::Duration,
};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;

    write!(&mut tty, "{}", escape::HIDE_CURSOR)?;
    tty.flush()?;

    let n = 3;

    for i in 0..n {
        write!(
            &mut tty,
            "{}{}",
            escape::CLEAR_LINE,
            escape::MoveToColumn(0)
        )?;
        write!(&mut tty, "{}", n - i)?;
        tty.flush()?;
        thread::sleep(Duration::from_secs(1));
    }

    write!(
        &mut tty,
        "{}{}{}",
        escape::CLEAR_LINE,
        escape::MoveToColumn(0),
        escape::SHOW_CURSOR,
    )?;
    tty.flush()?;

    Ok(())
}
