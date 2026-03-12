use illuso_term::{KeyboardEnhancementFlags as KEF, *};
use std::io::{self, Write};

pub struct Terminal {
    tty: Tty,
    reader: Reader,
}

impl Terminal {
    pub fn init() -> io::Result<Self> {
        let mut tty = Tty::init()?;
        let reader = Reader::new();

        // TODO: Query whether Kitty keyboard protocol is supported (if possible?).

        // NOTE: All these flags must be enabled to get full keyboard functionality, since the
        // raw/legacy keys are intentionally not supported.
        write!(
            tty,
            "{}",
            KeyboardEnhancementFlagsPush(
                KEF::DISAMBIGUATE | KEF::REPORT_EVENTS | KEF::REPORT_ALL_KEYS
            )
        )?;

        // TODO: Query whether keyboard enhancement flags took effect.

        // TODO: Query whether in-band resize is supported.

        write!(tty, "{}", IN_BAND_RESIZE_SET)?;

        tty.flush()?;

        Ok(Self { tty, reader })
    }

    pub fn size(&self) -> io::Result<(u16, u16)> {
        self.tty.size()
    }

    pub fn read_event(&mut self) -> io::Result<Event> {
        self.reader.read_event(&mut self.tty)
    }
}

impl Write for Terminal {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.tty.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.tty.flush()
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = write!(self.tty, "{}", IN_BAND_RESIZE_RESET);
        let _ = write!(self.tty, "{}", KeyboardEnhancementFlagsPop);
        let _ = self.tty.flush();
    }
}
