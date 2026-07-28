#![expect(unsafe_code)]

use std::{
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
    mem::{self, MaybeUninit},
    os::fd::{AsRawFd as _, RawFd},
};

pub struct Tty {
    reader: File,
    writer: BufWriter<File>,
    original_termios: Option<libc::termios>,
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
        self.original_termios = Some(enable_raw_mode(self.reader.as_raw_fd())?);

        Ok(())
    }

    pub fn disable_raw_mode(&mut self) -> io::Result<()> {
        if let Some(original_termios) = mem::take(&mut self.original_termios) {
            disable_raw_mode(self.reader.as_raw_fd(), &original_termios)?;
        }

        Ok(())
    }

    pub fn size(&self) -> io::Result<(u16, u16)> {
        let mut winsize = MaybeUninit::<libc::winsize>::uninit();
        // SAFETY: `winsize` points to valid, writable memory of the right size.
        let ret = unsafe {
            libc::ioctl(
                self.reader.as_raw_fd(),
                libc::TIOCGWINSZ,
                winsize.as_mut_ptr(),
            )
        };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: `ioctl` succeeded, so `winsize` is initialized.
        let winsize = unsafe { winsize.assume_init() };
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

fn tcgetattr(fd: RawFd) -> io::Result<libc::termios> {
    let mut termios = MaybeUninit::<libc::termios>::uninit();
    // SAFETY: `termios` points to valid, writable memory of the right size.
    let ret = unsafe { libc::tcgetattr(fd, termios.as_mut_ptr()) };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: `tcgetattr` succeeded, so `termios` is initialized.
    Ok(unsafe { termios.assume_init() })
}

fn tcsetattr(fd: RawFd, termios: &libc::termios) -> io::Result<()> {
    // SAFETY: `termios` is a valid, initialized `termios` value.
    let ret = unsafe { libc::tcsetattr(fd, libc::TCSAFLUSH, termios) };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

fn enable_raw_mode(fd: RawFd) -> io::Result<libc::termios> {
    let cooked_termios = tcgetattr(fd)?;
    let mut raw_termios = cooked_termios;
    // SAFETY: `raw_termios` is a valid, initialized `termios` value.
    unsafe { libc::cfmakeraw(&raw mut raw_termios) };
    tcsetattr(fd, &raw_termios)?;
    Ok(cooked_termios)
}

fn disable_raw_mode(fd: RawFd, termios: &libc::termios) -> io::Result<()> {
    tcsetattr(fd, termios)?;
    Ok(())
}
