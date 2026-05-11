#![no_main]

use indigo_core::{
    buffer::Buffer,
    editor::{Action, Editor},
};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Action>)| {
    let (text, actions) = input;
    let mut editor = Editor::from(Buffer::from(Rope::from(text)));
    for action in actions {
        editor.handle_action(&action);
    }
});
