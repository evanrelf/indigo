use crate::editor::Editor;
use crossterm::event::KeyEvent;
use downcast_rs::{impl_downcast, Downcast};

pub mod command;
pub mod insert;
pub mod normal;

pub trait Mode: Downcast {
    fn mode_name(&self) -> &'static str;

    fn handle_key(
        &mut self,
        editor: &mut Editor,
        key_event: KeyEvent,
    ) -> Result<bool, anyhow::Error>;
}

impl_downcast!(Mode);
