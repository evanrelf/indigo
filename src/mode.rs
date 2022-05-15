use crate::{editor::Editor, key::Key};
use downcast_rs::{impl_downcast, Downcast};

pub mod command;
pub mod insert;
pub mod normal;

pub trait Mode: Downcast {
    fn mode_name(&self) -> &'static str;

    fn handle_key(&mut self, editor: &mut Editor, key: Key) -> Result<(), anyhow::Error>;
}

impl_downcast!(Mode);
