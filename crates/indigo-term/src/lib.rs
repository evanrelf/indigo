pub mod input;
pub mod output;
#[cfg(feature = "ratatui")]
pub mod ratatui;
mod terminal;
mod tty;

pub use crate::{
    terminal::Terminal,
    tty::{Reader, Tty},
};
