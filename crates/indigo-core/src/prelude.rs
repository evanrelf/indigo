#[doc(inline)]
pub use indigo_kernel::{display_width::DisplayWidth, edit::Edit};

#[doc(inline)]
pub use crate::{
    buffer::{Buffer, BufferKind},
    client::{Client, ClientMut},
    cursor::{Cursor, CursorEdit},
    key::{Key, KeyCode, KeyModifiers, Keys},
    mode::Mode,
    range::{Range, RangeEdit},
    rope::RopeExt,
    server::Server,
    session::{Action, Event, KeyEvent, KeyEventKind, Session},
    text::Text,
    window::{Window, WindowMut},
};

pub use ropey::{Rope, RopeSlice};
