use rustix::termios::{self, OptionalActions, Termios};
use std::{
    fs, io,
    ops::{Deref, DerefMut},
    os::fd::OwnedFd,
};

pub struct Tty {
    // The TTY's file descriptor (i.e. `/dev/tty`)
    fd: OwnedFd,
    // Original terminal state before enabling raw mode
    termios: Termios,
}

impl Tty {
    pub fn init() -> io::Result<Self> {
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/tty")?;
        let fd = OwnedFd::from(file);
        let termios = enable_raw_mode(&fd)?;
        Ok(Self { fd, termios })
    }

    pub fn restore(self) {
        drop(self);
    }
}

impl Drop for Tty {
    fn drop(&mut self) {
        let _ = disable_raw_mode(&self.fd, &self.termios);
    }
}

impl Deref for Tty {
    type Target = OwnedFd;
    fn deref(&self) -> &Self::Target {
        &self.fd
    }
}

impl DerefMut for Tty {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.fd
    }
}

fn enable_raw_mode(fd: &OwnedFd) -> io::Result<Termios> {
    let mut termios = termios::tcgetattr(fd)?;
    termios.make_raw();
    termios::tcsetattr(fd, OptionalActions::Flush, &termios)?;
    Ok(termios)
}

fn disable_raw_mode(fd: &OwnedFd, termios: &Termios) -> io::Result<()> {
    termios::tcsetattr(fd, OptionalActions::Flush, termios)?;
    Ok(())
}
