#![warn(
    clippy::cast_lossless,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::must_use_candidate,
    clippy::use_self
)]

pub mod buffer;
pub mod conversion;
pub mod direction;
pub mod editor;
pub mod mode;
pub mod position;
pub mod range;
pub mod rope;
pub mod selection;
