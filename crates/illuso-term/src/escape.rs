use bitflags::bitflags;
use std::fmt::{self, Display};

// Clearing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// Cursor

pub struct MoveUp(pub u16);

impl Display for MoveUp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}A", self.0)
    }
}

pub struct MoveDown(pub u16);

impl Display for MoveDown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}B", self.0)
    }
}

pub struct MoveForward(pub u16);

impl Display for MoveForward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}C", self.0)
    }
}

pub struct MoveBackward(pub u16);

impl Display for MoveBackward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}D", self.0)
    }
}

pub struct MoveTo(pub u16, pub u16);

impl Display for MoveTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{};{}H", self.0, self.1)
    }
}

pub struct MoveToColumn(pub u16);

impl Display for MoveToColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}G", self.0)
    }
}

pub const SAVE_CURSOR: &str = "\x1b[s";
pub const RESTORE_CURSOR: &str = "\x1b[u";
pub const HIDE_CURSOR: &str = "\x1b[?25l";
pub const SHOW_CURSOR: &str = "\x1b[?25h";

// Modes

pub struct QueryMode(pub u16);

impl Display for QueryMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}$p", self.0)
    }
}

pub struct SetMode(pub u16);

impl Display for SetMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}h", self.0)
    }
}

pub struct ResetMode(pub u16);

impl Display for ResetMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}l", self.0)
    }
}

// Primary Device Attributes (DA1)
// https://vt100.net/docs/vt510-rm/DA1.html

pub const DA1_QUERY: &str = "\x1b[c";

// Kitty keyboard protocol
// https://sw.kovidgoyal.net/kitty/keyboard-protocol/

bitflags! {
    #[derive(Debug, PartialEq)]
    pub struct KittyKeyboardFlags: u8 {
        const DISAMBIGUATE = 1;
        const REPORT_EVENTS = 2;
        const REPORT_ALTERNATES = 4; // Unsupported
        const REPORT_ALL_KEYS = 8;   // Supported, but only for keys we represent
        const REPORT_TEXT = 16;      // Ignored, but safe to enable
    }
}

impl Display for KittyKeyboardFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.bits())
    }
}

pub struct KittyKeyboardFlagsPush(pub KittyKeyboardFlags);

impl Display for KittyKeyboardFlagsPush {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[>{}u", self.0)
    }
}

pub struct KittyKeyboardFlagsPop;

impl Display for KittyKeyboardFlagsPop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[<1u")
    }
}

pub const KITTY_KEYBOARD_FLAGS_QUERY: &str = "\x1b[?u";

// Bracketed paste
// https://en.wikipedia.org/wiki/Bracketed-paste

pub const BRACKETED_PASTE_QUERY: QueryMode = QueryMode(2004);
pub const BRACKETED_PASTE_SET: SetMode = SetMode(2004);
pub const BRACKETED_PASTE_RESET: ResetMode = ResetMode(2004);
pub const BRACKETED_PASTE_BEGIN: &str = "\x1b[200~";
pub const BRACKETED_PASTE_END: &str = "\x1b[201~";

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const IN_BAND_RESIZE_QUERY: QueryMode = QueryMode(2048);
pub const IN_BAND_RESIZE_SET: SetMode = SetMode(2048);
pub const IN_BAND_RESIZE_RESET: ResetMode = ResetMode(2048);

// Synchronized output
// https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md

pub const SYNC_UPDATE_QUERY: QueryMode = QueryMode(2026);
pub const SYNC_UPDATE_SET: SetMode = SetMode(2026);
pub const SYNC_UPDATE_RESET: ResetMode = ResetMode(2026);
