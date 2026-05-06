use crate::{
    editor::Editor,
    mode::{command, insert, normal, prompt, seek},
};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Normal(normal::Action),
    Insert(insert::Action),
    Prompt(prompt::Action),
    Command(command::Action),
    // TODO: Replace this with multi-key mappings in normal mode
    Seek(seek::Action),
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::Normal(action) => normal::handle_action(editor, action),
        Action::Insert(action) => insert::handle_action(editor, action),
        Action::Prompt(action) => prompt::handle_action(editor, action),
        Action::Command(action) => command::handle_action(editor, action),
        Action::Seek(action) => seek::handle_action(editor, action),
    }
}
