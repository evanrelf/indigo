pub mod actions;

use camino::Utf8PathBuf;
use ropey::Rope;
use std::{fs::File, io::BufReader};

pub struct Editor {
    pub text: Rope,
    // Byte offset
    pub cursor: usize,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            cursor: 0,
        })
    }
}
