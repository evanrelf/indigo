use crate::{selection::Selection, validate::Validate as _};
use ropey::Rope;
use std::{
    fs::File,
    io::{self, BufReader, BufWriter},
    path::{Path, PathBuf},
};

#[derive(Default)]
pub struct Buffer {
    pub path: Option<PathBuf>,
    contents: Rope,
    is_modified: bool,
    selection: Selection,
}

impl Buffer {
    #[must_use]
    pub fn contents(&self) -> &Rope {
        &self.contents
    }

    #[must_use]
    pub fn is_modified(&self) -> bool {
        self.is_modified
    }

    #[must_use]
    pub fn selection(&self) -> &Selection {
        &self.selection
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

    pub fn save(&mut self) -> Result<(), io::Error> {
        if let Some(path) = &self.path {
            let file = File::open(path)?;
            let writer = BufWriter::new(file);
            let result = self.contents.write_to(writer);
            if result.is_ok() {
                self.is_modified = false;
            }
            result
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "no path associated with buffer",
            ))
        }
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(
            {
                let mut valid = true;
                for range in self.selection().ranges() {
                    if !range.is_valid(&self.contents) {
                        valid = false;
                        break;
                    }
                }
                valid
            },
            "selection must be valid in the rope"
        )
    }
}
