use bitflags::bitflags;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct Key {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

#[derive(Debug, PartialEq)]
pub enum KeyCode {
    Esc,
    Backspace,
    Tab,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Char(char),
}

bitflags! {
    #[derive(Debug, PartialEq)]
    pub struct KeyModifiers: u8 {
        const SHIFT = 1;
        const ALT = 2;
        const CTRL = 4;
    }
}

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
