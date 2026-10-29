//! What you send to the terminal

use bitflags::bitflags;
use std::fmt::{self, Display};

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

pub const CURSOR_SAVE: &str = "\x1b[s";
pub const CURSOR_RESTORE: &str = "\x1b[u";
pub const CURSOR_HIDE: &str = "\x1b[?25l";
pub const CURSOR_SHOW: &str = "\x1b[?25h";

// Style

pub const BOLD: &str = "\x1b[1m";
pub const ITALIC: &str = "\x1b[3m";
pub const UNDERLINE: &str = "\x1b[4m";
pub const RESET: &str = "\x1b[0m";

pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    /// A color from the terminal's 256-color palette
    Indexed(u8),
    /// A 24-bit color
    Rgb(u8, u8, u8),
}

pub struct Fg(pub Color);

impl Display for Fg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Color::Black => write!(f, "\x1b[30m"),
            Color::Red => write!(f, "\x1b[31m"),
            Color::Green => write!(f, "\x1b[32m"),
            Color::Yellow => write!(f, "\x1b[33m"),
            Color::Blue => write!(f, "\x1b[34m"),
            Color::Magenta => write!(f, "\x1b[35m"),
            Color::Cyan => write!(f, "\x1b[36m"),
            Color::White => write!(f, "\x1b[37m"),
            Color::Indexed(index) => write!(f, "\x1b[38;5;{index}m"),
            Color::Rgb(r, g, b) => write!(f, "\x1b[38;2;{r};{g};{b}m"),
        }
    }
}

pub struct Bg(pub Color);

impl Display for Bg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Color::Black => write!(f, "\x1b[40m"),
            Color::Red => write!(f, "\x1b[41m"),
            Color::Green => write!(f, "\x1b[42m"),
            Color::Yellow => write!(f, "\x1b[43m"),
            Color::Blue => write!(f, "\x1b[44m"),
            Color::Magenta => write!(f, "\x1b[45m"),
            Color::Cyan => write!(f, "\x1b[46m"),
            Color::White => write!(f, "\x1b[47m"),
            Color::Indexed(index) => write!(f, "\x1b[48;5;{index}m"),
            Color::Rgb(r, g, b) => write!(f, "\x1b[48;2;{r};{g};{b}m"),
        }
    }
}

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

pub const BRACKETED_PASTE_MODE: u16 = 2004;
pub const BRACKETED_PASTE_QUERY: ModeQuery = ModeQuery(BRACKETED_PASTE_MODE);
pub const BRACKETED_PASTE_SET: ModeSet = ModeSet(BRACKETED_PASTE_MODE);
pub const BRACKETED_PASTE_RESET: ModeReset = ModeReset(BRACKETED_PASTE_MODE);

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

// Color scheme
// https://github.com/contour-terminal/contour/blob/master/docs/vt-extensions/color-palette-update-notifications.md

pub const COLOR_SCHEME_QUERY: &str = "\x1b[?996n";
pub const COLOR_SCHEME_UPDATES_MODE: u16 = 2031;
pub const COLOR_SCHEME_UPDATES_QUERY: ModeQuery = ModeQuery(COLOR_SCHEME_UPDATES_MODE);
pub const COLOR_SCHEME_UPDATES_SET: ModeSet = ModeSet(COLOR_SCHEME_UPDATES_MODE);
pub const COLOR_SCHEME_UPDATES_RESET: ModeReset = ModeReset(COLOR_SCHEME_UPDATES_MODE);

// Kitty keyboard protocol
// https://sw.kovidgoyal.net/kitty/keyboard-protocol/

bitflags! {
    #[derive(Debug, PartialEq)]
    pub struct KeyboardEnhancementFlags: u8 {
        const DISAMBIGUATE = 1;
        const REPORT_EVENTS = 2;
        const REPORT_ALTERNATES = 4; // Unsupported
        const REPORT_ALL_KEYS = 8;   // Supported, but only for keys we represent
        const REPORT_TEXT = 16;      // Ignored, but safe to enable
    }
}

impl Display for KeyboardEnhancementFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.bits())
    }
}

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
