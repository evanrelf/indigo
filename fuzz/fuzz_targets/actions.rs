#![no_main]

use indigo_core::{
    buffer::Buffer,
    session::{Action, Session},
};
use libfuzzer_sys::fuzz_target;
use ropey::Rope;

fuzz_target!(|input: (&str, Vec<Action>)| {
    let (text, actions) = input;
    let mut session = Session::from(Buffer::from(Rope::from(text)));
    for action in actions {
        session.handle_action(&action);
    }
});
