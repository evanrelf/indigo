use std::fmt::{self, Display};

// Cursor movement

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

// Erasing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// Synchronized output

pub const BEGIN_SYNC_UPDATE: &str = "\x1b[?2026h";
pub const END_SYNC_UPDATE: &str = "\x1b[?2026l";

// Cursor visibility

pub const HIDE_CURSOR: &str = "\x1b[?25l";
pub const SHOW_CURSOR: &str = "\x1b[?25h";

// Terminal queries

pub const QUERY_CURSOR_POSITION: &str = "\x1b[6n";
pub const QUERY_TERMINAL_SIZE: &str = "\x1b[18t";

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const ENABLE_IN_BAND_RESIZE: &str = "\x1b[?2048h";
pub const DISABLE_IN_BAND_RESIZE: &str = "\x1b[?2048l";
pub const QUERY_IN_BAND_RESIZE: &str = "\x1b[?2048$p";
