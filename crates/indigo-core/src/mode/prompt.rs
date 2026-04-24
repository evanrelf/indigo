use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{Mode, normal::enter_normal_mode},
    text::Text,
};
use ropey::Rope;
use std::sync::{Arc, Mutex};

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

pub fn handle_event_prompt(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Prompt(_prompt_mode) = &editor.mode else {
        panic!("Not in prompt mode")
    };

    let mut handled = true;

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<c-b>") => move_left(editor),
            _ if is(key, "<c-f>") => move_right(editor),
            _ if is(key, "<c-a>") => move_to_start(editor),
            _ if is(key, "<c-e>") => move_to_end(editor),
            _ if is(key, "<c-u>") => delete_to_start(editor),
            _ if is(key, "<c-k>") => delete_to_end(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => exec(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, char::from(c)),
            _ => handled = false,
        },
    }

    editor.pending_keys.clear();

    handled
}

pub fn enter_prompt_mode(
    editor: &mut Editor,
    prompt: &'static str,
    handler: impl FnMut(&mut Editor, &str) + Send + 'static,
) {
    editor.mode = Mode::Prompt(State::new(prompt, handler));
}

fn move_left(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_left(1);
}

fn move_right(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_right(1);
}

fn move_to_start(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_to_start();
}

fn move_to_end(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().move_to_end();
}

fn delete_to_start(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_mut();
    while !cursor.is_at_start() {
        cursor.delete_before();
    }
}

fn delete_to_end(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_mut();
    while !cursor.is_at_end() {
        cursor.delete_after();
    }
}

fn insert_char(editor: &mut Editor, char: char) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().insert_char(char);
}

pub fn paste(editor: &mut Editor, text: &str) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_mut().insert(text);
}

fn delete_before(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    if prompt_mode.cursor().is_at_start() {
        enter_normal_mode(editor);
    } else {
        prompt_mode.cursor_mut().delete_before();
    }
}

fn exec(editor: &mut Editor) {
    let Mode::Prompt(prompt_mode) = &mut editor.mode else {
        panic!("Not in prompt mode")
    };
    let text = prompt_mode.rope().to_string();
    let handler = prompt_mode
        .handler
        .take()
        .expect("Handler is always present");
    handler.lock().unwrap()(editor, &text);
    enter_normal_mode(editor);
}
