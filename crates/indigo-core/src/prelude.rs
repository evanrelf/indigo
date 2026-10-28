#[doc(inline)]
pub use indigo_kernel::{display_width::DisplayWidth, edit::Edit};

#[doc(inline)]
pub use indigo_key::{Key, KeyCode, KeyModifiers, Keys};

#[doc(inline)]
pub use crate::{
    buffer::{Buffer, BufferKind},
    client::{Action, Client, ClientMut, Event, KeyEvent, KeyEventKind},
    cursor::{Cursor, CursorEdit},
    mode::Mode,
    range::{Range, RangeEdit},
    rope::RopeExt,
    server::Server,
    session::Session,
    text::Text,
    window::{Window, WindowMut},
};

pub use ropey::{Rope, RopeSlice};
