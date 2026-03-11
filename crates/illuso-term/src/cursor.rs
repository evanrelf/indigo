use std::fmt::{self, Display};

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
