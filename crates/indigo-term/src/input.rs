//! Decoding what the terminal sends

pub mod logical;
pub mod physical;

use crate::{
    input::physical::{Token, Tokenizer},
    output::KeyboardEnhancementFlags,
};
use bitflags::bitflags;
use std::mem;

// Decoder

#[derive(Default)]
pub struct Decoder {
    tokenizer: Tokenizer,
    paste: Vec<u8>,
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
        self.tokenizer.feed(bytes)
    }

    /// Decode the next event, or return `None` if more bytes are needed.
    pub fn next_event(&mut self) -> Option<Event> {
        while let Some(token) = self.tokenizer.next_token() {
            match token {
                Token::PasteBegin => {}
                Token::PasteChunk(chunk) => self.paste.extend_from_slice(&chunk),
                Token::PasteEnd => {
                    let paste = mem::take(&mut self.paste);
                    return Some(Event::Paste(String::from_utf8_lossy(&paste).into_owned()));
                }
                token => return Some(logical::decode(token)),
            }
        }
        None
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
    /// Text pasted while bracketed paste is enabled, with any invalid UTF-8 replaced. Use a
    /// [`Tokenizer`] to get the raw bytes instead, as they arrive.
    ///
    /// <https://en.wikipedia.org/wiki/Bracketed-paste>
    Paste(String),
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

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::generators as gs;

    /// Feed `input` to a decoder in chunks split at `splits`, taking events after each chunk.
    fn decode_chunked(input: &[u8], splits: &[usize]) -> Vec<Event> {
        let mut decoder = Decoder::new();
        let mut events = Vec::new();
        let mut start = 0;
        for end in splits.iter().copied().chain([input.len()]) {
            let mut chunk = &input[start..end];
            start = end;
            while !chunk.is_empty() {
                let fed = decoder.feed(chunk);
                assert!(fed > 0, "decoder buffer full");
                chunk = &chunk[fed..];
                while let Some(event) = decoder.next_event() {
                    events.push(event);
                }
            }
        }
        events
    }

    #[hegel::test(test_cases = 1_000)]
    fn paste_is_decoded_regardless_of_chunking(tc: hegel::TestCase) {
        // A partial end marker, an escape sequence, a run long enough to fill a chunk, multi-byte
        // UTF-8, and invalid UTF-8. None of these combine to form the end marker.
        let piece = gs::sampled_from(vec![
            &b"\x1b[20"[..],
            b"\x1b[A",
            b"a run of pasted text that fills a whole chunk",
            "é".as_bytes(),
            b"\xff",
        ]);
        let text = tc.draw(gs::vecs(piece)).concat();
        let input = [
            BRACKETED_PASTE_BEGIN.as_bytes(),
            &text,
            BRACKETED_PASTE_END.as_bytes(),
            b"\x1b[97u",
        ]
        .concat();
        let mut splits = tc.draw(gs::vecs(gs::integers::<usize>().max_value(input.len())));
        splits.sort_unstable();
        assert_eq!(
            decode_chunked(&input, &splits),
            vec![
                Event::Paste(String::from_utf8_lossy(&text).into_owned()),
                Event::KeyPress(Key {
                    code: KeyCode::Char('a'),
                    modifiers: KeyModifiers::empty(),
                }),
            ]
        );
    }

    #[test]
    fn paste_can_be_larger_than_the_buffer() {
        let text = "a run of pasted text that fills a whole chunk\n".repeat(100);
        let input = [BRACKETED_PASTE_BEGIN, &text, BRACKETED_PASTE_END].concat();
        assert_eq!(
            decode_chunked(input.as_bytes(), &[]),
            vec![Event::Paste(text)]
        );
    }
}
