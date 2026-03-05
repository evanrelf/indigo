use rustix::termios::{self, OptionalActions, Termios};
use std::{io, sync::Mutex};

// Raw mode

static ORIGINAL_TERMIOS: Mutex<Option<Termios>> = Mutex::new(None);

pub fn enable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if original.is_some() {
        return Ok(());
    }

    let mut termios = termios::tcgetattr(io::stdin())?;
    *original = Some(termios.clone());

    termios.make_raw();
    termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;

    Ok(())
}

pub fn disable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if let Some(termios) = original.take() {
        termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;
    }

    Ok(())
}
