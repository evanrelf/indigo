pub mod actions;
mod cursor;
mod display_width;
mod editor;
mod event;
mod hooks;
mod key;
mod keymap;
mod mode;
mod ot;
mod range;
pub mod range2; // TODO: Publish the module's stuff, not the module itself
mod rope;

pub use crate::{
    cursor::{Cursor, CursorExt, CursorMut},
    display_width::DisplayWidth,
    editor::Editor,
    event::{Event, EventKind},
    hooks::{Hook, Hooks},
    key::{Key, KeyCode, KeyModifier, Keys},
    keymap::Keymap,
    mode::Mode,
    ot::{Edit, EditSeq, Error},
    range::{Range, RangeExt, RangeMut},
    rope::RopeExt,
};
