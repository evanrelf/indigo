mod buffer;
pub mod command;
mod conversion;
mod editor;
mod file;
mod key;
mod mode;
mod non_empty;
mod position;
mod range;
mod reflect;
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
    key::types::{self as key_types, key},
    key::{Key, KeyCode, KeyModifier, Keys},
    mode::{CommandMode, InsertMode, Mode, NormalMode},
    position::Position,
    range::Range,
    reflect::Reflect,
    rope::RopeExt,
    selection::{Selection, SelectionMut},
    settings::Settings,
    window::{Window, WindowKey, WindowMut},
};
