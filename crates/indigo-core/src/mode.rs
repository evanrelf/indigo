pub mod command;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod seek;

#[derive(Clone)]
pub enum Mode {
    Normal(normal::State),
    Insert(insert::State),
    Prompt(prompt::State),
    // TODO: Replace this with multi-key mappings in normal mode
    Seek(seek::State),
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal(normal::State::default())
    }
}
