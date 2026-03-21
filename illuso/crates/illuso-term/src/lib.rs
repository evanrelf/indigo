mod cursor;
mod event;
mod key;
mod parser;
mod reader;
mod style;
mod tty;

use std::fmt::{self, Display};

pub use crate::{cursor::*, event::Event, key::*, reader::Reader, style::*, tty::Tty};

// Clearing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// Modes

pub struct ModeQuery(pub u16);

impl Display for ModeQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}$p", self.0)
    }
}

pub struct ModeSet(pub u16);

impl Display for ModeSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}h", self.0)
    }
}

pub struct ModeReset(pub u16);

impl Display for ModeReset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}l", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub enum ModeSetting {
    NotRecognized,
    Set,
    Reset,
    PermanentlySet,
    PermanentlyReset,
}

impl ModeSetting {
    #[must_use]
    pub fn is_recognized(&self) -> bool {
        !matches!(self, Self::NotRecognized)
    }

    #[must_use]
    pub fn is_enabled(&self) -> bool {
        matches!(self, Self::Set | Self::PermanentlySet)
    }

    #[must_use]
    pub fn is_disabled(&self) -> bool {
        matches!(
            self,
            Self::NotRecognized | Self::Reset | Self::PermanentlyReset
        )
    }

    #[must_use]
    pub fn is_configurable(&self) -> bool {
        matches!(self, Self::Set | Self::Reset)
    }
}

// Primary Device Attributes (DA1)
// https://vt100.net/docs/vt510-rm/DA1.html

pub const DA1_QUERY: &str = "\x1b[c";

// Bracketed paste
// https://en.wikipedia.org/wiki/Bracketed-paste

pub const BRACKETED_PASTE_MODE: u16 = 2004;
pub const BRACKETED_PASTE_QUERY: ModeQuery = ModeQuery(BRACKETED_PASTE_MODE);
pub const BRACKETED_PASTE_SET: ModeSet = ModeSet(BRACKETED_PASTE_MODE);
pub const BRACKETED_PASTE_RESET: ModeReset = ModeReset(BRACKETED_PASTE_MODE);
pub const BRACKETED_PASTE_BEGIN: &str = "\x1b[200~";
pub const BRACKETED_PASTE_END: &str = "\x1b[201~";

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const IN_BAND_RESIZE_MODE: u16 = 2048;
pub const IN_BAND_RESIZE_QUERY: ModeQuery = ModeQuery(IN_BAND_RESIZE_MODE);
pub const IN_BAND_RESIZE_SET: ModeSet = ModeSet(IN_BAND_RESIZE_MODE);
pub const IN_BAND_RESIZE_RESET: ModeReset = ModeReset(IN_BAND_RESIZE_MODE);

// Synchronized output
// https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md

pub const SYNC_UPDATE_MODE: u16 = 2026;
pub const SYNC_UPDATE_QUERY: ModeQuery = ModeQuery(SYNC_UPDATE_MODE);
pub const SYNC_UPDATE_SET: ModeSet = ModeSet(SYNC_UPDATE_MODE);
pub const SYNC_UPDATE_RESET: ModeReset = ModeReset(SYNC_UPDATE_MODE);
