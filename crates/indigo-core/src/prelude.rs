#[doc(inline)]
pub use crate::{
    buffer::{Buffer, BufferKind},
    cursor::{Cursor, CursorMut},
    edit::OperationSeq,
    editor::{Action, Editor, Event, KeyEvent, KeyEventKind},
    key::{Key, KeyCode, KeyModifiers, Keys},
    mode::Mode,
    range::{Range, RangeMut},
    rope::{Bias, DisplayWidth, RopeExt},
    text::Text,
    window::{Window, WindowMut},
};

pub use ropey::{Rope, RopeSlice};
