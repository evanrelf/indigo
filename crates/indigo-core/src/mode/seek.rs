use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::KeyCode,
    mode::{Mode, normal::enter_normal_mode},
};
use std::num::NonZeroUsize;

pub enum SeekSelect {
    Move,
    Extend,
}

pub enum SeekInclude {
    Until,
    Onto,
}

pub enum SeekDirection {
    Prev,
    Next,
}

pub struct SeekMode {
    pub count: Option<NonZeroUsize>,
    /// Move and create a new selection, or extend the current one?
    pub select: SeekSelect,
    /// Include seek target in selection, or stop just short?
    pub include: SeekInclude,
    /// Seek forward or backward?
    pub direction: SeekDirection,
}

pub fn handle_event_seek(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Seek(_seek_mode) = &editor.mode else {
        panic!("Not in seek mode")
    };

    let Event::Key(KeyEvent { key, .. }) = event;

    let mut key = *key;
    key.normalize();

    if let KeyCode::Char(c) = key.code
        && key.modifiers.is_empty()
    {
        seek(editor, c);
    } else if let KeyCode::Return = key.code
        && key.modifiers.is_empty()
    {
        // TODO: Handle Windows line endings?
        seek(editor, b'\n');
    } else if let KeyCode::Tab = key.code
        && key.modifiers.is_empty()
    {
        seek(editor, b'\t');
    }

    enter_normal_mode(editor);

    true
}

pub fn enter_seek_mode(
    editor: &mut Editor,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
) {
    editor.mode = Mode::Seek(SeekMode {
        count: editor.mode.count(),
        select,
        include,
        direction,
    });
}

fn seek(editor: &mut Editor, byte: u8) {
    use SeekDirection::{Next, Prev};
    use SeekInclude::{Onto, Until};
    use SeekSelect::{Extend, Move};

    let Mode::Seek(seek_mode) = &editor.mode else {
        panic!("Not in seek mode")
    };

    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();

    editor.buffer.selection_mut().for_each_mut(|mut range| {
        match (&seek_mode.select, &seek_mode.include, &seek_mode.direction) {
            (Move, Until, Prev) => range.move_until_prev_byte(byte, count),
            (Extend, Until, Prev) => range.extend_until_prev_byte(byte, count),
            (Move, Until, Next) => range.move_until_next_byte(byte, count),
            (Extend, Until, Next) => range.extend_until_next_byte(byte, count),
            (Move, Onto, Prev) => range.move_onto_prev_byte(byte, count),
            (Extend, Onto, Prev) => range.extend_onto_prev_byte(byte, count),
            (Move, Onto, Next) => range.move_onto_next_byte(byte, count),
            (Extend, Onto, Next) => range.extend_onto_next_byte(byte, count),
        }
    });

    editor.mode.set_count(None);
}
