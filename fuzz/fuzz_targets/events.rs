#![no_main]

use indigo_core::{buffer::Buffer, client::Event, server::Server};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Event>)| {
    let (text, events) = input;
    let mut server = Server::new(
        "/home/fuzz".into(),
        "/home/fuzz/project".into(),
        Buffer::from(Rope::from(text)),
    );
    for event in events {
        server.handle_event(event).unwrap();
    }
});
