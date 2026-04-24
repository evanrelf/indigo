use crate::{
    editor::Editor,
    key::Key,
    mode::{Mode, goto, insert, normal, prompt, seek},
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
        editor.pending_keys.push(*key);
    }

    let handled = match event {
        Event::Key(_) => match editor.mode {
            Mode::Normal(_) => normal::handle_keys(editor),
            Mode::Seek(_) => seek::handle_keys(editor),
            Mode::Goto => goto::handle_keys(editor),
            Mode::Insert(_) => insert::handle_keys(editor),
            Mode::Prompt(_) => prompt::handle_keys(editor),
        },
    };

    Ok(handled)
}
