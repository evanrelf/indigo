use crate::escape::KittyKeyboardFlags;
use bitflags::bitflags;
use tinyvec::TinyVec;

#[derive(Debug, PartialEq)]
pub enum Event {
    KeyPress(Key),
    Resize {
        height: u16,
        width: u16,
    },
    KittyKeyboardFlags(KittyKeyboardFlags),
    /// Device attributes <https://vt100.net/docs/vt510-rm/DA1.html>
    Da1,
    /// Report mode <https://vt100.net/docs/vt510-rm/DECRPM.html>
    Decrpm {
        mode: u16,
        value: u8,
    },
    UnknownCsi {
        parameter_bytes: TinyVec<[u8; 32]>,
        intermediate_bytes: TinyVec<[u8; 2]>,
        final_byte: u8,
    },
    UnknownOsc {
        data_bytes: TinyVec<[u8; 256]>,
    },
}

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
