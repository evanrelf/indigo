use crate::selection::Selection;
use ropey::Rope;
use std::{
    fs::File,
    io::{self, BufReader, BufWriter},
    path::{Path, PathBuf},
};

// INVARIANTS:
// - Selection must be valid in the rope
#[derive(Default)]
pub struct Buffer {
    path: Option<PathBuf>,
    contents: Rope,
    selection: Selection,
}

impl Buffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open<P>(path: P) -> Result<Self, io::Error>
    where
        P: AsRef<Path>,
    {
        let path_buf = path.as_ref().to_path_buf();
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        Ok(Self {
            path: Some(path_buf),
            contents: Rope::from_reader(reader)?,
            ..Self::default()
        })
    }

    pub fn save(&self) -> Result<(), io::Error> {
        if let Some(path) = &self.path {
            let file = File::open(path)?;
            let writer = BufWriter::new(file);
            self.contents.write_to(writer)
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "no path associated with buffer",
            ))
        }
    }
}
