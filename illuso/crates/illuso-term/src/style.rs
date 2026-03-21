use std::fmt::{self, Display};

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
}

impl Color {
    fn sgr_offset(&self) -> u8 {
        match self {
            Color::Black => 0,
            Color::Red => 1,
            Color::Green => 2,
            Color::Yellow => 3,
            Color::Blue => 4,
            Color::Magenta => 5,
            Color::Cyan => 6,
            Color::White => 7,
        }
    }
}

pub struct Fg(pub Color);

impl Display for Fg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}m", 30 + self.0.sgr_offset())
    }
}

pub struct Bg(pub Color);

impl Display for Bg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}m", 40 + self.0.sgr_offset())
    }
}
