use illuso_term::{KeyboardEnhancementFlags as KKF, *};
use std::io::{self, Write as _};

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
                KKF::DISAMBIGUATE | KKF::REPORT_EVENTS | KKF::REPORT_ALL_KEYS
            )
        )?;

        // TODO: Query whether keyboard enhancement flags took effect.

        // TODO: Query whether in-band resize is supported. If it is, enable it. If it isn't, panic.

        tty.flush()?;

        Ok(Self { tty, reader })
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = write!(self.tty, "{}", KeyboardEnhancementFlagsPop);
        let _ = self.tty.flush();
    }
}
