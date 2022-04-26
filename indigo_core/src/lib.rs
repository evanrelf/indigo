#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

mod buffer;
mod editor;
mod position;
mod range;
mod selection;

#[doc(inline)]
pub use crate::{buffer::*, editor::*, position::*, range::*, selection::*};
