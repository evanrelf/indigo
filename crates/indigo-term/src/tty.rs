#![expect(unsafe_code)]

use crate::input::{Decoder, Event};
use std::{
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
    mem::{self, MaybeUninit},
    os::fd::{AsRawFd as _, RawFd},
};
use tinyvec::ArrayVec;

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
        if self.original_termios.is_some() {
            return Ok(());
        }

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
        let winsize = winsize(self.reader.as_raw_fd())?;
        Ok((winsize.ws_row, winsize.ws_col))
    }

    pub fn pixel_size(&self) -> io::Result<(u16, u16)> {
        let winsize = winsize(self.reader.as_raw_fd())?;
        Ok((winsize.ws_ypixel, winsize.ws_xpixel))
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

fn winsize(fd: RawFd) -> io::Result<libc::winsize> {
    let mut winsize = MaybeUninit::<libc::winsize>::uninit();
    // SAFETY: `winsize` points to valid, writable memory of the right size.
    let ret = unsafe { libc::ioctl(fd, libc::TIOCGWINSZ, winsize.as_mut_ptr()) };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: `ioctl` succeeded, so `winsize` is initialized.
    Ok(unsafe { winsize.assume_init() })
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

#[derive(Default)]
pub struct Reader {
    decoder: Decoder,
    unfed: ArrayVec<[u8; 256]>,
}

impl Reader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Read the next event from the input source. Blocks until a complete event is available.
    pub fn read_event(&mut self, input: &mut impl Read) -> io::Result<Event> {
        loop {
            if let Some(event) = self.decoder.next_event() {
                return Ok(event);
            }

            if self.unfed.is_empty() {
                let bytes_read = input.read(self.unfed.grab_spare_slice_mut())?;
                if bytes_read == 0 {
                    return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "input closed"));
                }
                self.unfed.set_len(bytes_read);
            }

            let bytes_fed = self.decoder.feed(&self.unfed);
            if bytes_fed == 0 {
                return Err(io::Error::other("reader buffer full"));
            }
            self.unfed.drain(..bytes_fed);
        }
    }
}
