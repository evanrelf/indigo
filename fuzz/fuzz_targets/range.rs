#![no_main]

use indigo_core::{
    range::{self, Action, RangeSnapshot, RangeView},
    rope::RopeExt as _,
    text::Text,
};
use libfuzzer_sys::fuzz_target;
use std::array;

#[derive(Debug, arbitrary::Arbitrary)]
enum FuzzAction {
    Range(Action),
    Save { slot: u8 },
    Restore { slot: u8 },
}

fuzz_target!(|input: (&str, usize, usize, Vec<FuzzAction>)| {
    let (text, tail, head, actions) = input;
    let text = Text::from(text);
    let valid_tail = tail <= text.len() && text.is_grapheme_boundary(tail);
    let valid_head = head <= text.len() && text.is_grapheme_boundary(head);
    let Ok(mut range) = RangeView::try_from((text, tail, head)) else {
        assert!(!valid_tail || !valid_head);
        return;
    };
    assert!(valid_tail);
    assert!(valid_head);
    let mut snapshots: [Option<RangeSnapshot>; 4] = array::from_fn(|_| None);
    for action in actions {
        match action {
            FuzzAction::Range(action) => {
                range::handle_action(&mut range, &action);
            }
            FuzzAction::Save { slot } => {
                snapshots[usize::from(slot) % snapshots.len()] = Some(range.save());
            }
            FuzzAction::Restore { slot } => {
                if let Some(snapshot) = &snapshots[usize::from(slot) % snapshots.len()] {
                    range.restore(snapshot);
                }
            }
        }

        range.assert_invariants().unwrap();
        assert_eq!(range.byte_length(), range.slice().len());
        assert_eq!(range.is_backward(), !range.is_forward());
        assert!(range.start().byte_offset() <= range.end().byte_offset());
    }
});
