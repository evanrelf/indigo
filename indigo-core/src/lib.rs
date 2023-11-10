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
pub mod conversion;
pub mod direction;
pub mod editor;
pub mod mode;
pub mod position;
pub mod range;
pub mod rope;
pub mod selection;

pub use crate::{
    buffer::Buffer,
    conversion::Conversion,
    direction::Direction,
    editor::Editor,
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    rope::RopeExt,
    selection::Selection,
};
