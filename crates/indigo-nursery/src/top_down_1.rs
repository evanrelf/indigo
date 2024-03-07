// Ambitious, exploring possibilities

#![allow(clippy::type_complexity)]

use slotmap::SlotMap;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

struct Editor {
    buffers: SlotMap<BufferKey, Buffer>,
    // NOTE: Should `windows`, `layout`, and `focus` all live in some larger "window manager"
    // struct?
    windows: SlotMap<WindowKey, Window>,
    // NOTE: Should `layout` live in the TUI? (should the TUI even be separate from the beginning?)
    layout: Layout,
    focus: WindowKey,
    // NOTE: Could use buffers? Editing the contents of registers would be easy.
    registers: HashMap<char, String>,
    mode: Vec<Mode>,
    mappings: KeyRouter,
    hooks: EventRouter,
    settings: GlobalSettings,
}

// NOTE: Should `contents` and `selection` live in their own struct?
// NOTE: How should text and selection undo history work? I like how Kakoune has a separate
// selection-specific undo/redo history.
struct Buffer {
    path: Option<PathBuf>,
    contents: String,
    selection: Selection,
    settings: BufferSettings,
    // TODO: Undo history
}

slotmap::new_key_type! { struct BufferKey; }

struct Window {
    buffer: BufferKey,
    vertical_scroll: usize,
    horizontal_scroll: usize,
    settings: WindowSettings,
}

slotmap::new_key_type! { struct WindowKey; }

// Window layout.
// NOTE: Would be cool to support multiple layout structures/algorithms, like XMonad.
struct Layout;

enum Mode {
    Normal { count: usize },
    Insert,
    // NOTE: Could be a buffer? Only shows one line, history stored as previous lines.
    Command { command: String, cursor: Position },
}

struct KeyRouter;

struct EventRouter;

struct Settings;

struct GlobalSettings;

struct BufferSettings;

struct WindowSettings;

struct Key {
    modifiers: HashSet<KeyModifier>,
    code: KeyCode,
}

enum KeyModifier {}

enum KeyCode {}

struct Selection {
    ranges: Vec<Range>,
    primary: usize,
}

struct Range {
    anchor: Position,
    cursor: Position,
    target_column: Option<usize>,
}

struct Position {
    line: usize,
    column: usize,
}

enum Event {
    EditorCreate,
    EditorDestroy,
    BufferCreate(BufferKey),
    BufferDestroy(BufferKey),
    WindowCreate(WindowKey),
    WindowDestroy(WindowKey),
    // etc.
}

enum Command {
    Quit { exit_code: u8 },
}
