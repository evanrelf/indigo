pub mod actions;
mod display_width;
mod editor;
mod event;
mod hooks;
mod key;
mod mode;
mod ot;
mod range;
mod rope;

pub use crate::{
    display_width::DisplayWidth,
    editor::Editor,
    event::{Event, EventKind},
    hooks::{Hook, Hooks},
    key::{Key, KeyCode, KeyModifier, Keys},
    mode::Mode,
    ot::{Edit, EditSeq, Error},
    range::{Range, RangeExt, RangeMut},
    rope::RopeExt,
};
