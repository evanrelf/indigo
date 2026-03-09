use rustix::termios::{self, OptionalActions, Termios};
use std::{
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
    os::fd::{AsFd as _, BorrowedFd},
};

pub struct Tty {
    reader: File,
    writer: BufWriter<File>,
    original_termios: Termios,
}

impl Tty {
    pub fn init() -> io::Result<Self> {
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/tty")?;
        let writer = BufWriter::new(file.try_clone()?);
        let original_termios = enable_raw_mode(file.as_fd())?;
        Ok(Self {
            reader: file,
            writer,
            original_termios,
        })
    }
}

impl Drop for Tty {
    fn drop(&mut self) {
        let _ = disable_raw_mode(self.reader.as_fd(), &self.original_termios);
    }
}

impl Read for Tty {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

impl Write for Tty {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

fn enable_raw_mode(fd: BorrowedFd<'_>) -> io::Result<Termios> {
    let mut termios = termios::tcgetattr(fd)?;
    let cooked_termios = termios.clone();
    termios.make_raw();
    termios::tcsetattr(fd, OptionalActions::Flush, &termios)?;
    Ok(cooked_termios)
}

fn disable_raw_mode(fd: BorrowedFd<'_>, termios: &Termios) -> io::Result<()> {
    termios::tcsetattr(fd, OptionalActions::Flush, termios)?;
    Ok(())
}
