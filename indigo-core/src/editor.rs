use crate::{File, FileKey, Mode};
use slotmap::SlotMap;

#[derive(Debug)]
pub struct Editor {
    files: SlotMap<FileKey, File>,
    current_file: FileKey,
    mode: Mode,
}

impl Default for Editor {
    fn default() -> Self {
        let mut files = SlotMap::with_key();
        let current_file = files.insert(File::default());
        Self {
            files,
            current_file,
            mode: Mode::default(),
        }
    }
}

impl Editor {
    #[must_use]
    pub fn get_file(&self, file_key: FileKey) -> Option<&File> {
        self.files.get(file_key)
    }

    #[must_use]
    pub fn get_file_mut(&mut self, file_key: FileKey) -> Option<&mut File> {
        self.files.get_mut(file_key)
    }

    #[must_use]
    pub fn current_file(&self) -> &File {
        self.files.get(self.current_file).unwrap()
    }

    #[must_use]
    pub fn current_file_mut(&mut self) -> &mut File {
        self.files.get_mut(self.current_file).unwrap()
    }

    #[must_use]
    pub fn current_file_key(&self) -> FileKey {
        self.current_file
    }

    #[must_use]
    pub fn files(&self) -> &SlotMap<FileKey, File> {
        &self.files
    }

    // TODO: Should `mode` just be `pub`? Or are there invariants that need to be enforced, so this
    // should be more restrictive?

    #[must_use]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    #[must_use]
    pub fn mode_mut(&mut self) -> &mut Mode {
        &mut self.mode
    }

    #[must_use]
    pub fn insert_file(&mut self, file: File) -> FileKey {
        self.files.insert(file)
    }

    // pub fn remove_file(&mut self, file_key: FileKey) -> anyhow::Result<()> {
    //     if !self.files.contains_key(file_key) {
    //         anyhow::bail!("File does not exist");
    //     }
    //     if self.files.len() == 1 {
    //         anyhow::bail!("Cannot remove last file");
    //     }
    //     // TODO: Update `current_file` to stay valid
    //     self.files.remove(file_key);
    //     Ok(())
    // }

    pub fn set_current_file(&mut self, file_key: FileKey) -> anyhow::Result<()> {
        if !self.files.contains_key(file_key) {
            anyhow::bail!("File does not exist");
        }
        self.current_file = file_key;
        Ok(())
    }

    pub fn assert_valid(&self) {
        assert!(
            self.files.get(self.current_file).is_some(),
            "`current_file` index is valid"
        );

        for file in self.files.values() {
            file.assert_valid();
        }
    }
}
