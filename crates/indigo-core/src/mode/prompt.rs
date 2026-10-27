#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    cursor::{Cursor, CursorEdit, CursorMove, CursorState},
    keymap::{Keymap, KeymapResult, Mapping, keymap},
    mode::{Mode, normal},
    text::Text,
};
use ropey::{Rope, RopeSlice};
use std::sync::{Arc, LazyLock, Mutex};

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Clone)]
pub struct State {
    pub prompt: &'static str,
    text: Text,
    cursor: CursorState,
    // TODO: Consider changing this back to `Box<dyn FnOnce(..)>`. Either write a weird `Clone` impl
    // that makes `handler = None` or add a new trait for deriving view state?
    #[expect(clippy::type_complexity)]
    handler: Option<Arc<Mutex<dyn FnMut(&mut ClientMut, &str) + Send>>>,
}

impl State {
    #[must_use]
    pub fn new(
        prompt: &'static str,
        handler: impl FnMut(&mut ClientMut, &str) + Send + 'static,
    ) -> Self {
        Self {
            prompt,
            text: Text::default(),
            cursor: CursorState::default(),
            handler: Some(Arc::new(Mutex::new(handler))),
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.text.rope()
    }

    /// The prompt's content, without the `Text` invariant's trailing newline.
    #[must_use]
    pub fn content(&self) -> RopeSlice<'_> {
        let rope = self.text.rope();
        assert_eq!(
            rope.byte(rope.len() - 1),
            b'\n',
            "Prompt text keeps its trailing newline"
        );
        rope.slice(..rope.len() - 1)
    }

    pub fn cursor(&self) -> Cursor<'_> {
        let cursor = Cursor::new(&self.text, &self.cursor)
            .expect("Command mode text and cursor state are always kept valid");
        cursor.assert_invariants().unwrap();
        cursor
    }

    pub fn cursor_move(&mut self) -> CursorMove<'_> {
        CursorMove::new(&self.text, &mut self.cursor)
            .expect("Command mode text and cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    pub fn cursor_edit(&mut self) -> CursorEdit<'_> {
        CursorEdit::new(&mut self.text, &mut self.cursor)
            .expect("Command mode text and cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        let _ = Cursor::new(&self.text, &self.cursor)?;
        Ok(())
    }
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
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
        "<esc>" => vec![EnterNormalMode], /// Cancel
        "<ret>" => vec![Exec], /// Submit
        "<c-b>" => vec![MoveLeft], /// Move left
        "<c-f>" => vec![MoveRight], /// Move right
        "<c-a>" => vec![MoveToStart], /// Move to start
        "<c-e>" => vec![MoveToEnd], /// Move to end
        "<c-u>" => vec![DeleteToStart], /// Delete to start
        "<c-k>" => vec![DeleteToEnd], /// Delete to end
        "<bs>" => vec![DeleteBefore], /// Delete before cursor
        "<any>" => Mapping::Any(|key| Some(vec![InsertChar(char::from(key.typed_char()?))])), /// Insert character
    }
});

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::EnterNormalMode => normal::enter(client),
        Action::MoveLeft => move_left(client),
        Action::MoveRight => move_right(client),
        Action::MoveToStart => move_to_start(client),
        Action::MoveToEnd => move_to_end(client),
        Action::DeleteToStart => delete_to_start(client),
        Action::DeleteToEnd => delete_to_end(client),
        Action::DeleteBefore => delete_before(client),
        Action::InsertChar(c) => insert_char(client, *c),
        Action::Exec => exec(client),
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    match KEYMAP.get_keys(&client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.pending_keys.clear();
            handle_actions(client, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            client.pending_keys.clear();
            handle_actions(client, &actions);
            true
        }
        KeymapResult::Unmapped => {
            client.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(
    client: &mut ClientMut,
    prompt: &'static str,
    handler: impl FnMut(&mut ClientMut, &str) + Send + 'static,
) {
    client.mode = Mode::Prompt(State::new(prompt, handler));
    client.count = None;
}

fn move_left(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_move().move_left(1);
}

fn move_right(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_move().move_right(1);
}

fn move_to_start(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_move().move_to_start();
}

fn move_to_end(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_move().move_to_end();
}

fn delete_to_start(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_edit();
    while cursor.delete_before().is_some() {}
}

fn delete_to_end(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    let mut cursor = prompt_mode.cursor_edit();
    while cursor.delete_after().is_some() {}
}

fn insert_char(client: &mut ClientMut, char: char) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_edit().insert_char(char);
}

pub fn paste(client: &mut ClientMut, text: &str) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    prompt_mode.cursor_edit().insert(text);
}

fn delete_before(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    if prompt_mode.cursor().is_at_start() {
        normal::enter(client);
    } else {
        prompt_mode.cursor_edit().delete_before();
    }
}

fn exec(client: &mut ClientMut) {
    let Mode::Prompt(prompt_mode) = &mut client.mode else {
        panic!("Not in prompt mode")
    };
    let text = prompt_mode.content().to_string();
    let handler = prompt_mode
        .handler
        .take()
        .expect("Handler is always present");
    handler.lock().unwrap()(client, &text);
    normal::enter(client);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn content_strips_trailing_newline() {
        let mut state = State::new("test", |_, _| {});
        // The empty prompt is "\n" internally, but its content is empty.
        assert_eq!(state.content().to_string(), "");
        assert!(state.cursor().is_at_end());

        state.cursor_edit().insert("abc");
        assert_eq!(state.rope().to_string(), "abc\n");
        assert_eq!(state.content().to_string(), "abc");

        // Typing at the "end" inserts before the invariant newline.
        state.cursor_move().move_to_end();
        assert!(state.cursor().is_at_end());
        state.cursor_edit().insert_char('!');
        assert_eq!(state.content().to_string(), "abc!");

        // Kill-to-end deletes everything after the cursor except the invariant newline.
        state.cursor_move().move_to_start();
        let mut cursor = state.cursor_edit();
        while cursor.delete_after().is_some() {}
        drop(cursor);
        assert_eq!(state.content().to_string(), "");
    }
}
