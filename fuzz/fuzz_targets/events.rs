#![no_main]

use indigo_core::{
    buffer::Buffer,
    session::{Event, Session},
};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Event>)| {
    let (text, events) = input;
    let mut session = Session::from(Buffer::from(Rope::from(text)));
    for event in events {
        session.handle_event(event).unwrap();
    }
});
