use bitflags::bitflags;

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
    pub struct KittyKeyboardFlags: u8 {
        const DISAMBIGUATE = 1;
        const REPORT_EVENTS = 2;
        const REPORT_ALTERNATES = 4; // Unsupported
        const REPORT_ALL_KEYS = 8;   // Supported, but only for keys we represent
        const REPORT_TEXT = 16;      // Ignored, but safe to enable
    }
}
