mod cursor;
mod event;
mod key;
mod parser;
mod reader;
mod tty;

use std::fmt::{self, Display};

pub use crate::{cursor::*, event::Event, key::*, reader::Reader, tty::Tty};

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

// Primary Device Attributes (DA1)
// https://vt100.net/docs/vt510-rm/DA1.html

pub const DA1_QUERY: &str = "\x1b[c";

// Bracketed paste
// https://en.wikipedia.org/wiki/Bracketed-paste

pub const BRACKETED_PASTE_QUERY: ModeQuery = ModeQuery(2004);
pub const BRACKETED_PASTE_SET: ModeSet = ModeSet(2004);
pub const BRACKETED_PASTE_RESET: ModeReset = ModeReset(2004);
pub const BRACKETED_PASTE_BEGIN: &str = "\x1b[200~";
pub const BRACKETED_PASTE_END: &str = "\x1b[201~";

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const IN_BAND_RESIZE_QUERY: ModeQuery = ModeQuery(2048);
pub const IN_BAND_RESIZE_SET: ModeSet = ModeSet(2048);
pub const IN_BAND_RESIZE_RESET: ModeReset = ModeReset(2048);

// Synchronized output
// https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md

pub const SYNC_UPDATE_QUERY: ModeQuery = ModeQuery(2026);
pub const SYNC_UPDATE_SET: ModeSet = ModeSet(2026);
pub const SYNC_UPDATE_RESET: ModeReset = ModeReset(2026);
