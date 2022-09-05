#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

pub mod buffer;
pub mod command_line;
pub mod commands;
pub mod editor;
pub mod mode;
pub mod ot;
pub mod position;
pub mod range;
pub mod selection;

pub use crate::{
    buffer::Buffer,
    command_line::CommandLine,
    commands::{parse, Cli, Command},
    editor::Editor,
    mode::{CommandMode, GotoMode, InsertMode, Mode, NormalMode},
    ot::{Affinity, Operations},
    position::Position,
    range::Range,
    selection::Selection,
};
