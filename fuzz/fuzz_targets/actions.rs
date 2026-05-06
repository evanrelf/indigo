#![no_main]

use indigo_core::{
    action::{Action, handle_action},
    buffer::Buffer,
    editor::Editor,
};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Action>)| {
    let (text, actions) = input;
    let mut editor = Editor::from(Buffer::from(Rope::from(text)));
    for action in actions {
        handle_action(&mut editor, &action);
    }
});
