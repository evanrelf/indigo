pub mod escape;
pub mod event;
pub mod key;
pub mod parser;
pub mod reader;
pub mod tty;

pub use crate::{
    event::Event,
    key::{Key, KeyCode, KeyModifiers, KittyKeyboardFlags},
    reader::Reader,
    tty::Tty,
};
