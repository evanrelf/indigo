#![no_main]

use indigo_core::{buffer::Buffer, client::Action, server::Server};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Action>)| {
    let (text, actions) = input;
    let mut server = Server::from(Buffer::from(Rope::from(text)));
    for action in actions {
        server.handle_action(&action);
    }
});
