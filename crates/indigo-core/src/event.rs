use crate::{FileKey, ModeKind, WindowKey};

// Kakoune's `hook` events: https://github.com/mawww/kakoune/blob/master/doc/pages/hooks.asciidoc#default-hooks
// Neovim's `autocmd` events: https://neovim.io/doc/user/autocmd.html#autocmd-events

pub enum Event {
    EditorCreate,
    EditorDestroy,
    FileCreate(FileKey),
    FileWritePre(FileKey),
    FileWritePost(FileKey),
    FileDestroy(FileKey),
    WindowCreate(WindowKey),
    WindowFocus(WindowKey),
    WindowBlur(WindowKey),
    WindowDestroy(WindowKey),
    ModeChange { old: ModeKind, new: ModeKind },
    Error(String),
    Custom(String),
}
