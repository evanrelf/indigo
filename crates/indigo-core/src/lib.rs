mod buffer;
pub mod command;
mod conversion;
mod editor;
mod event;
mod file;
mod key;
mod mode;
mod position;
mod range;
mod rope;
mod selection;
pub mod settings;
mod window;

#[doc(inline)]
pub use crate::{
    buffer::Buffer,
    command::Command,
    conversion::Conversion,
    editor::Editor,
    event::Event,
    file::{File, FileKey},
    key::{Key, KeyCode, KeyModifier, Keys},
    mode::{CommandMode, InsertMode, Mode, ModeKind, NormalMode},
    position::Position,
    range::Range,
    rope::RopeExt,
    selection::{Selection, SelectionMut},
    settings::Settings,
    window::{Window, WindowKey, WindowMut},
};
