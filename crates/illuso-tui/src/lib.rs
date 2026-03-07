pub mod event;

use rustix::termios::{self, OptionalActions, Termios};
use std::{
    fmt::{self, Display},
    io,
    sync::Mutex,
};

// Raw mode

static ORIGINAL_TERMIOS: Mutex<Option<Termios>> = Mutex::new(None);

pub fn enable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if original.is_some() {
        return Ok(());
    }

    let mut termios = termios::tcgetattr(io::stdin())?;
    *original = Some(termios.clone());

    termios.make_raw();
    termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;

    Ok(())
}

pub fn disable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if let Some(termios) = original.take() {
        termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;
    }

    Ok(())
}

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

// Clearing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const QUERY_IN_BAND_RESIZE: &str = "\x1b[?2048$p";
pub const ENABLE_IN_BAND_RESIZE: &str = "\x1b[?2048h";
pub const DISABLE_IN_BAND_RESIZE: &str = "\x1b[?2048l";

// Synchronized output
// https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md

pub const QUERY_SYNC_UPDATE: &str = "\x1b[?2026$p";
pub const BEGIN_SYNC_UPDATE: &str = "\x1b[?2026h";
pub const END_SYNC_UPDATE: &str = "\x1b[?2026l";
