#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

pub mod buffer;
pub mod editor;
pub mod entangle;
pub mod position;
pub mod range;
pub mod selection;
pub mod valid;

pub use crate::{
    buffer::Buffer,
    editor::Editor,
    entangle::{Entangle, Entangled},
    position::Position,
    range::Range,
    selection::Selection,
    valid::Valid,
};
