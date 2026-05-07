#![no_main]

use indigo_core::{
    cursor::{self, Action, Cursor, CursorView},
    rope::RopeExt as _,
    text::Text,
};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: (&str, usize, Vec<Action>)| {
    let (text, byte_offset, actions) = input;
    let text = Text::from(text);
    let byte_offset = text.ceil_grapheme_boundary(byte_offset);
    let mut cursor = CursorView::try_from((text, byte_offset)).unwrap();
    for action in actions {
        cursor::handle_action(&mut cursor, &action);
        let _ = Cursor::new(cursor.text(), cursor.state()).unwrap();
    }
});
