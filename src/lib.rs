#![warn(
    clippy::use_self,
    // TODO
    // clippy::cast_possible_truncation,
    // clippy::cast_possible_wrap,
    // clippy::cast_sign_loss,
)]

pub mod buffer;
pub mod command;
pub mod cursor;
pub mod direction;
pub mod editor;
pub mod operand;
pub mod ot;
pub mod range;
pub mod rope;
pub mod selection;
pub mod terminal;
