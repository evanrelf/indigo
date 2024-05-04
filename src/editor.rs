use camino::Utf8PathBuf;
use ropey::Rope;
use std::{fs::File, io::BufReader};

pub struct Editor {
    text: Rope,
    // Byte offset
    pub cursor: usize,
    // Line offset
    pub vertical_scroll: usize,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            cursor: 0,
            vertical_scroll: 0,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }
}
