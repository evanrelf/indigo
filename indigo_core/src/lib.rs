#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

pub mod buffer;
pub mod editor;
pub mod position;
pub mod range;
pub mod selection;

pub use crate::{
    buffer::Buffer, editor::Editor, position::Position, range::Range, selection::Selection,
};
