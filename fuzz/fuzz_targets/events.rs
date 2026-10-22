#![no_main]

use indigo_core::{buffer::Buffer, server::Server, session::Event};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Event>)| {
    let (text, events) = input;
    let mut server = Server::from(Buffer::from(Rope::from(text)));
    for event in events {
        server.handle_event(event).unwrap();
    }
});
