#[doc(inline)]
pub use crate::{
    buffer::{Buffer, BufferKind},
    cursor::{Cursor, CursorMut},
    display_width::DisplayWidth,
    edit::EditSeq,
    editor::Editor,
    event::{Event, KeyEvent, KeyEventKind},
    key::{Key, KeyCode, KeyModifiers, Keys},
    mode::{
        Mode, command::CommandMode, goto::GotoMode, insert::InsertMode, normal::NormalMode,
        seek::SeekMode,
    },
    range::{Range, RangeMut},
    rope::RopeExt,
    text::Text,
    window::{Window, WindowMut},
};

pub use ropey::{Rope, RopeSlice};
