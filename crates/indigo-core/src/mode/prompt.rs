use crate::{
    cursor::{Cursor, CursorMut, CursorState},
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{Mode, normal::enter_normal_mode},
    text::Text,
};
use ropey::Rope;

pub struct PromptMode {
    prompt: &'static str,
    text: Text,
    cursor: CursorState,
    #[expect(clippy::type_complexity)]
    handler: Option<Box<dyn FnMut(&mut Editor, &str)>>,
}

impl PromptMode {
    #[must_use]
    pub fn new(prompt: &'static str, handler: impl FnMut(&mut Editor, &str) + 'static) -> Self {
        Self {
            prompt,
            text: Text::default(),
            cursor: CursorState::default(),
            handler: Some(Box::new(handler)),
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

    // TODO: Add mappings for cursor movement.
    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => exec(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, char::from(c)),
            _ => handled = false,
        },
    }

    handled
}

pub fn enter_prompt_mode(
    editor: &mut Editor,
    prompt: &'static str,
    handler: impl FnMut(&mut Editor, &str) + 'static,
) {
    editor.mode = Mode::Prompt(PromptMode::new(prompt, handler));
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
    let mut handler = prompt_mode
        .handler
        .take()
        .expect("Handler is always present");
    handler(editor, &text);
    enter_normal_mode(editor);
}
