#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
// TODO: Remove
#![allow(dead_code)]

mod buffer;
pub mod command;
mod editor;
mod key;
mod mode;
mod position;
mod range;
mod rope;
mod selection;

pub use crate::{
    buffer::Buffer,
    command::Command,
    editor::Editor,
    key::{Key, KeyCode, KeyModifier, Keys},
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    rope::RopeExt,
    selection::Selection,
};
