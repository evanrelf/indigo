#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
// TODO: Remove
#![allow(dead_code)]

pub mod buffer;
pub mod command;
pub mod conversion;
pub mod direction;
pub mod editor;
pub mod key;
pub mod mode;
pub mod position;
pub mod range;
pub mod rope;
pub mod selection;

pub use crate::{
    buffer::Buffer,
    command::Command,
    conversion::Conversion,
    direction::Direction,
    editor::Editor,
    key::{Key, KeyCode, KeyModifier, Keys},
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    rope::RopeExt,
    selection::Selection,
};
