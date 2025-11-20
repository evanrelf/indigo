#[doc(inline)]
pub use crate::{
    buffer::{Buffer, BufferKind},
    cursor::{Cursor, CursorMut},
    display_width::DisplayWidth,
    editor::Editor,
    event::{Event, KeyEvent, KeyEventKind},
    key::{Key, KeyCode, KeyModifier, KeyModifiers, Keys},
    mode::{InsertMode, Mode, NormalMode, command::CommandMode, goto::GotoMode, seek::SeekMode},
    ot::EditSeq,
    range::{Range, RangeMut},
    rope::RopeExt,
    text::Text,
    window::{Window, WindowMut},
};

pub use ropey::{Rope, RopeSlice};
