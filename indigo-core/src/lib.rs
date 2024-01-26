mod buffer;
pub mod command;
mod conversion;
mod editor;
mod file;
mod key;
mod key_map;
mod mode;
mod non_empty;
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
    file::{File, FileKey},
    key::{Key, KeyCode, KeyModifier, Keys},
    key_map::KeyMap,
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    rope::RopeExt,
    selection::{Selection, SelectionMut},
    settings::Settings,
    window::{Window, WindowKey, WindowMut},
};
