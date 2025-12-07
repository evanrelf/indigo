use crate::{
    editor::Editor,
    key::Key,
    mode::{
        Mode, command::handle_event_command, goto::handle_event_goto, insert::handle_event_insert,
        normal::handle_event_normal, prompt::handle_event_prompt, seek::handle_event_seek,
    },
};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    Key(KeyEvent),
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub struct KeyEvent {
    pub key: Key,
    pub kind: KeyEventKind,
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub enum KeyEventKind {
    Press,
    Repeat,
    Release,
}

pub fn handle_event(editor: &mut Editor, mut event: Event) -> anyhow::Result<bool> {
    #[expect(irrefutable_let_patterns)]
    if let Event::Key(KeyEvent { key, .. }) = &mut event {
        key.normalize();
    }

    let handled = match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, &event),
        Mode::Seek(_) => handle_event_seek(editor, &event),
        Mode::Goto(_) => handle_event_goto(editor, &event),
        Mode::Insert(_) => handle_event_insert(editor, &event),
        Mode::Command(_) => handle_event_command(editor, &event)?,
        Mode::Prompt(_) => handle_event_prompt(editor, &event),
    };

    Ok(handled)
}
