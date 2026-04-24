use crate::mode::{command, insert, normal, prompt, seek};

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
