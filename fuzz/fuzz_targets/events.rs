#![no_main]

use indigo_core::{
    buffer::Buffer,
    editor::Editor,
    event::{Event, handle_event},
};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Event>)| {
    let (text, events) = input;
    let mut editor = Editor::from(Buffer::from(Rope::from(text)));
    for event in events {
        handle_event(&mut editor, event).unwrap();
    }
});
