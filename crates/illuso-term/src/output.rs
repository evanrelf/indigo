use crate::key::KeyboardEnhancementFlags;
use std::fmt::{self, Display};

// Clearing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// Cursor

pub struct CursorUp(pub u16);

impl Display for CursorUp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}A", self.0)
    }
}

pub struct CursorDown(pub u16);

impl Display for CursorDown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}B", self.0)
    }
}

pub struct CursorForward(pub u16);

impl Display for CursorForward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}C", self.0)
    }
}

pub struct CursorBackward(pub u16);

impl Display for CursorBackward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}D", self.0)
    }
}

pub struct CursorNextLine(pub u16);

impl Display for CursorNextLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}E", self.0)
    }
}

pub struct CursorPrevLine(pub u16);

impl Display for CursorPrevLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}F", self.0)
    }
}

pub struct CursorColumn(pub u16);

impl Display for CursorColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}G", self.0)
    }
}

pub struct CursorPosition(pub u16, pub u16);

impl Display for CursorPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{};{}H", self.0, self.1)
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

pub struct KeyboardEnhancementFlagsPush(pub KeyboardEnhancementFlags);

impl Display for KeyboardEnhancementFlagsPush {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[>{}u", self.0)
    }
}

pub struct KeyboardEnhancementFlagsPop;

impl Display for KeyboardEnhancementFlagsPop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[<1u")
    }
}

pub const KEYBOARD_ENHANCEMENT_FLAGS_QUERY: &str = "\x1b[?u";

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
