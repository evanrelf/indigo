use rustix::termios::{self, OptionalActions, Termios, tcgetwinsize};
use std::{
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
    mem,
    os::fd::{AsFd as _, BorrowedFd},
};

pub struct Tty {
    reader: File,
    writer: BufWriter<File>,
    original_termios: Option<Termios>,
}

impl Tty {
    pub fn init() -> io::Result<Self> {
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/tty")?;

        let writer = BufWriter::new(file.try_clone()?);

        Ok(Self {
            reader: file,
            writer,
            original_termios: None,
        })
    }

    pub fn enable_raw_mode(&mut self) -> io::Result<()> {
        self.original_termios = Some(enable_raw_mode(self.reader.as_fd())?);

        Ok(())
    }

    pub fn disable_raw_mode(&mut self) -> io::Result<()> {
        if let Some(original_termios) = mem::take(&mut self.original_termios) {
            disable_raw_mode(self.reader.as_fd(), &original_termios)?;
        }

        Ok(())
    }

    pub fn size(&self) -> io::Result<(u16, u16)> {
        let winsize = tcgetwinsize(&self.reader)?;
        Ok((winsize.ws_row, winsize.ws_col))
    }
}

impl Drop for Tty {
    fn drop(&mut self) {
        let _ = self.disable_raw_mode();
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
