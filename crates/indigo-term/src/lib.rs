pub mod input;
pub mod output;
mod terminal;
mod tty;

pub use crate::{
    terminal::Terminal,
    tty::{Reader, Tty},
};
