#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

pub mod buffer;
pub mod command_line;
pub mod editor;
pub mod entangle;
pub mod key;
pub mod mode;
pub mod position;
pub mod range;
pub mod selection;
pub mod validate;

pub use crate::{
    buffer::Buffer,
    command_line::CommandLine,
    editor::Editor,
    entangle::{Entangle, Entangled},
    key::{Key, KeyCode, KeyModifiers},
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    selection::Selection,
    validate::{Valid, Validate},
};
