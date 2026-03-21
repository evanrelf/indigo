use crate::{
    ModeSetting,
    key::{Key, KeyboardEnhancementFlags},
};
use tinyvec::TinyVec;

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
    /// Control Sequence Introducer
    ///
    /// <https://ghostty.org/docs/vt/concepts/sequences#csi>
    UnknownCsi {
        parameter_bytes: TinyVec<[u8; 32]>,
        intermediate_bytes: TinyVec<[u8; 2]>,
        final_byte: u8,
    },
    /// Operating System Command
    ///
    /// <https://ghostty.org/docs/vt/concepts/sequences#osc>
    UnknownOsc {
        data_bytes: TinyVec<[u8; 32]>,
    },
}
