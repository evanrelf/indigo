#![warn(
    clippy::use_self,
    // TODO
    // clippy::cast_possible_truncation,
    // clippy::cast_possible_wrap,
    // clippy::cast_sign_loss,
)]

pub mod buffer;
pub mod cursor;
pub mod editor;
pub mod ot;
pub mod position;
pub mod range;
pub mod selection;
pub mod terminal;
