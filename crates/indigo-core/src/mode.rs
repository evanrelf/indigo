pub mod command;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod seek;

#[derive(Clone, Default)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
    Prompt(prompt::State),
    // TODO: Replace this with multi-key mappings in normal mode
    Seek(seek::State),
}
