use rustix::termios::{self, OptionalActions, Termios};
use std::{
    fs::{self, File},
    io,
    ops::{Deref, DerefMut},
    os::fd::{AsFd as _, BorrowedFd},
};

pub struct Tty {
    // The TTY's file (i.e. `/dev/tty`)
    file: File,
    // Original terminal state before enabling raw mode
    termios: Termios,
}

impl Tty {
    pub fn init() -> io::Result<Self> {
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/tty")?;
        let termios = enable_raw_mode(file.as_fd())?;
        Ok(Self { file, termios })
    }

    pub fn restore(self) {
        drop(self);
    }
}

#[must_use]
pub fn init() -> Tty {
    Tty::init().unwrap()
}

impl Drop for Tty {
    fn drop(&mut self) {
        let _ = disable_raw_mode(self.file.as_fd(), &self.termios);
    }
}

impl Deref for Tty {
    type Target = File;
    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

impl DerefMut for Tty {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.file
    }
}

fn enable_raw_mode(fd: BorrowedFd<'_>) -> io::Result<Termios> {
    let mut termios = termios::tcgetattr(fd)?;
    termios.make_raw();
    termios::tcsetattr(fd, OptionalActions::Flush, &termios)?;
    Ok(termios)
}

fn disable_raw_mode(fd: BorrowedFd<'_>, termios: &Termios) -> io::Result<()> {
    termios::tcsetattr(fd, OptionalActions::Flush, termios)?;
    Ok(())
}
