#![no_main]

use indigo_core::{
    cursor::{self, Action, CursorSnapshot, CursorView},
    rope::RopeExt as _,
    text::Text,
};
use libfuzzer_sys::fuzz_target;
use std::array;

#[derive(Debug, arbitrary::Arbitrary)]
enum FuzzAction {
    Cursor(Action),
    Save { slot: u8 },
    Restore { slot: u8 },
}

fuzz_target!(|input: (&str, usize, Vec<FuzzAction>)| {
    let (text, byte_offset, actions) = input;
    let text = Text::from(text);
    let valid_byte_offset = byte_offset <= text.len() && text.is_grapheme_boundary(byte_offset);
    let Ok(mut cursor) = CursorView::try_from((text, byte_offset)) else {
        assert!(!valid_byte_offset);
        return;
    };
    assert!(valid_byte_offset);
    let mut snapshots: [Option<CursorSnapshot>; 4] = array::from_fn(|_| None);
    for action in actions {
        match action {
            FuzzAction::Cursor(action) => {
                cursor::handle_action(&mut cursor, &action);
            }
            FuzzAction::Save { slot } => {
                snapshots[usize::from(slot) % snapshots.len()] = Some(cursor.save());
            }
            FuzzAction::Restore { slot } => {
                if let Some(snapshot) = &snapshots[usize::from(slot) % snapshots.len()] {
                    cursor.restore(snapshot);
                }
            }
        }
        cursor.assert_invariants().unwrap();
    }
});
