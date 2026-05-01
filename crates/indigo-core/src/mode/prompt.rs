#![allow(clippy::enum_glob_use)]

use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    editor::Editor,
    key::KeyCode,
    keymap::{Keymap, KeymapResult, keymap},
    mode::Mode,
    text::Text,
};
use ropey::Rope;
use std::sync::{Arc, LazyLock, Mutex};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[derive(Clone)]
pub struct State {
    prompt: &'static str,
    text: Text,
    cursor: CursorState,
    // TODO: Consider changing this back to `Box<dyn FnOnce(..)>`. Either write a weird `Clone` impl
    // that makes `handler = None` or add a new trait for deriving view state?
    #[expect(clippy::type_complexity)]
    handler: Option<Arc<Mutex<dyn FnMut(&mut Editor, &str) + Send>>>,
}

impl State {
    #[must_use]
    pub fn new(
        prompt: &'static str,
        handler: impl FnMut(&mut Editor, &str) + Send + 'static,
    ) -> Self {
        Self {
            prompt,
            text: Text::default(),
            cursor: CursorState::default(),
            handler: Some(Arc::new(Mutex::new(handler))),
        }
    }

    #[must_use]
    pub fn prompt(&self) -> &str {
        self.prompt
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.text.rope()
    }

    pub fn cursor(&self) -> Cursor<'_> {
        let cursor = Cursor::new(&self.text, &self.cursor)
            .expect("Command mode text and cursor state are always kept valid");
        cursor.assert_invariants().unwrap();
        cursor
    }

    pub fn cursor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.cursor)
            .expect("Command mode text and cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    EnterNormalMode,
    MoveLeft,
    MoveRight,
    MoveToStart,
    MoveToEnd,
    DeleteToStart,
    DeleteToEnd,
    DeleteBefore,
    InsertChar(char),
    Exec,
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![EnterNormalMode],
        "<c-b>" => vec![MoveLeft],
        "<c-f>" => vec![MoveRight],
        "<c-a>" => vec![MoveToStart],
        "<c-e>" => vec![MoveToEnd],
        "<c-u>" => vec![DeleteToStart],
        "<c-k>" => vec![DeleteToEnd],
        "<bs>" => vec![DeleteBefore],
        "<ret>" => vec![Exec],
        keys => {
            if let [key] = keys
                && key.modifiers.is_empty()
                && let KeyCode::Char(c) = key.code
            {
                KeymapResult::Fallback(vec![InsertChar(char::from(c))])
            } else {
                KeymapResult::Unmapped
            }
        }
    }
});

pub fn handle_actions(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action(editor, action);
    }
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::EnterNormalMode => {
            editor.pop_mode();
        }
        Action::MoveLeft => move_left(editor),
        Action::MoveRight => move_right(editor),
        Action::MoveToStart => move_to_start(editor),
        Action::MoveToEnd => move_to_end(editor),
        Action::DeleteToStart => delete_to_start(editor),
        Action::DeleteToEnd => delete_to_end(editor),
        Action::DeleteBefore => delete_before(editor),
        Action::InsertChar(c) => insert_char(editor, *c),
        Action::Exec => exec(editor),
    }
}

pub fn handle_keys(editor: &mut Editor) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, &actions);
            true
        }
        KeymapResult::Unmapped => {
            editor.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn on_create(_editor: &mut Editor) {}

pub fn on_destroy(_editor: &mut Editor) {}

pub fn on_focus(_editor: &mut Editor) {}

pub fn on_blur(_editor: &mut Editor) {}

fn move_left(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_left(1);
}

fn move_right(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_right(1);
}

fn move_to_start(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_to_start();
}

fn move_to_end(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_to_end();
}

fn delete_to_start(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_mut();
    while !cursor.is_at_start() {
        cursor.delete_before();
    }
}

fn delete_to_end(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_mut();
    while !cursor.is_at_end() {
        cursor.delete_after();
    }
}

fn insert_char(editor: &mut Editor, char: char) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().insert_char(char);
}

pub fn paste(editor: &mut Editor, text: &str) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().insert(text);
}

fn delete_before(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    if prompt_mode.cursor().is_at_start() {
        editor.pop_mode();
    } else {
        prompt_mode.cursor_mut().delete_before();
    }
}

fn exec(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = editor.mode_mut() else {
        panic!("Not in prompt mode")
    };
    let text = prompt_mode.rope().to_string();
    let handler = prompt_mode
        .handler
        .take()
        .expect("Handler is always present");
    handler.lock().unwrap()(editor, &text);
    editor.pop_mode();
}
