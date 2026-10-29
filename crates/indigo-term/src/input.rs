//! Decoding what the terminal sends

mod logical;
pub mod physical;

use crate::{input::physical::Token, output::KeyboardEnhancementFlags};
use bitflags::bitflags;
use std::cmp::min;
use tinyvec::ArrayVec;
use winnow::{Parser, Partial, error::ErrMode};

// Decoder

#[derive(Default)]
pub struct Decoder {
    buffer: ArrayVec<[u8; 1024]>,
}

impl Decoder {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add as many bytes as fit, returning how many were added. Take events with
    /// [`Decoder::next_event`] to make room for more.
    #[must_use]
    pub fn feed(&mut self, bytes: &[u8]) -> usize {
        let count = min(bytes.len(), self.buffer.capacity() - self.buffer.len());
        self.buffer.extend_from_slice(&bytes[..count]);
        count
    }

    /// Decode the next event, or return `None` if more bytes are needed.
    pub fn next_event(&mut self) -> Option<Event> {
        loop {
            self.skip_garbage();

            if self.buffer.is_empty() {
                return None;
            }

            let input = Partial::new(self.buffer.as_slice());

            match logical::event.parse_peek(input) {
                Ok((remaining, event)) => {
                    let consumed = self.buffer.len() - remaining.len();
                    self.buffer.drain(..consumed);
                    return Some(event);
                }
                Err(ErrMode::Incomplete(_)) => return None,
                Err(_) => {
                    // Malformed sequence, skip the leading 0x1b and retry
                    self.buffer.drain(..1);
                }
            }
        }
    }

    /// Skip any non-ESC bytes at the front of the buffer. These are garbage bytes that don't start
    /// any recognized sequence.
    fn skip_garbage(&mut self) {
        let garbage_count = self
            .buffer
            .iter()
            .position(|&byte| byte == /* ESC */ 0x1B)
            .unwrap_or(self.buffer.len());
        if garbage_count > 0 {
            self.buffer.drain(..garbage_count);
        }
    }
}

// Events

#[derive(Debug, PartialEq)]
pub enum Event {
    KeyPress(Key),
    KeyRepeat(Key),
    KeyRelease(Key),
    Resize {
        height: u16,
        width: u16,
    },
    /// Kitty keyboard protocol progressive enhancement flags
    ///
    /// <https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement>
    KeyboardEnhancementFlags(KeyboardEnhancementFlags),
    /// Primary device attributes
    ///
    /// <https://vt100.net/docs/vt510-rm/DA1.html>
    Da1,
    /// Report mode
    ///
    /// <https://vt100.net/docs/vt510-rm/DECRPM.html>
    Decrpm {
        mode: u16,
        setting: ModeSetting,
    },
    /// Color scheme report, sent in response to a query or when the color scheme changes
    ///
    /// <https://github.com/contour-terminal/contour/blob/master/docs/vt-extensions/color-palette-update-notifications.md>
    ColorScheme(ColorScheme),
    /// A sequence that isn't recognized
    Unknown(Token),
}

// Keys

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

// Modes

#[derive(Debug, PartialEq)]
pub enum ModeSetting {
    NotRecognized,
    Set,
    Reset,
    PermanentlySet,
    PermanentlyReset,
}

impl ModeSetting {
    #[must_use]
    pub fn is_recognized(&self) -> bool {
        !matches!(self, Self::NotRecognized)
    }

    #[must_use]
    pub fn is_enabled(&self) -> bool {
        matches!(self, Self::Set | Self::PermanentlySet)
    }

    #[must_use]
    pub fn is_disabled(&self) -> bool {
        matches!(
            self,
            Self::NotRecognized | Self::Reset | Self::PermanentlyReset
        )
    }

    #[must_use]
    pub fn is_configurable(&self) -> bool {
        matches!(self, Self::Set | Self::Reset)
    }
}

// Color scheme
// https://github.com/contour-terminal/contour/blob/master/docs/vt-extensions/color-palette-update-notifications.md

#[derive(Debug, PartialEq)]
pub enum ColorScheme {
    Dark,
    Light,
}

// Bracketed paste
// https://en.wikipedia.org/wiki/Bracketed-paste

pub const BRACKETED_PASTE_BEGIN: &str = "\x1b[200~";
pub const BRACKETED_PASTE_END: &str = "\x1b[201~";
