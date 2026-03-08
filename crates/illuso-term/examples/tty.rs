use illuso_term::{escape, tty};
use std::{
    io::{self, Write as _},
    thread,
    time::Duration,
};

fn main() -> io::Result<()> {
    let mut tty = tty::init();

    write!(&mut tty.writer, "{}", escape::HIDE_CURSOR)?;
    tty.writer.flush()?;

    let n = 3;

    for i in 0..n {
        write!(
            &mut tty.writer,
            "{}{}",
            escape::CLEAR_LINE,
            escape::MoveToColumn(0)
        )?;
        write!(&mut tty.writer, "{}", n - i)?;
        tty.writer.flush()?;
        thread::sleep(Duration::from_secs(1));
    }

    write!(
        &mut tty.writer,
        "{}{}{}",
        escape::CLEAR_LINE,
        escape::MoveToColumn(0),
        escape::SHOW_CURSOR,
    )?;
    tty.writer.flush()?;

    Ok(())
}
