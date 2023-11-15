use crate::{File, FileKey, Mode, Window, WindowKey};
use slotmap::SlotMap;

#[derive(Debug)]
pub struct Editor {
    files: SlotMap<FileKey, File>,
    windows: SlotMap<WindowKey, Window>,
    current_window: WindowKey,
    mode: Mode,
}

impl Editor {
    #[must_use]
    pub fn new(file: File) -> Self {
        let mut files = SlotMap::with_key();
        let mut windows = SlotMap::with_key();
        let file_key = files.insert(file);
        let window_key = windows.insert(Window::new(file_key));
        Self {
            files,
            windows,
            current_window: window_key,
            mode: Mode::default(),
        }
    }

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
        self.files.get(self.current_file_key()).unwrap()
    }

    #[must_use]
    pub fn current_file_mut(&mut self) -> &mut File {
        self.files.get_mut(self.current_file_key()).unwrap()
    }

    #[must_use]
    pub fn current_file_key(&self) -> FileKey {
        self.current_window().file()
    }

    pub fn insert_file(&mut self, file: File) -> FileKey {
        self.files.insert(file)
    }

    // TODO: remove_file

    #[must_use]
    pub fn files(&self) -> &SlotMap<FileKey, File> {
        &self.files
    }

    #[must_use]
    pub fn get_window(&self, window_key: WindowKey) -> Option<&Window> {
        self.windows.get(window_key)
    }

    #[must_use]
    pub fn get_window_mut(&mut self, window_key: WindowKey) -> Option<&mut Window> {
        self.windows.get_mut(window_key)
    }

    #[must_use]
    pub fn current_window(&self) -> &Window {
        self.windows.get(self.current_window).unwrap()
    }

    #[must_use]
    pub fn current_window_mut(&mut self) -> &mut Window {
        self.windows.get_mut(self.current_window).unwrap()
    }

    #[must_use]
    pub fn current_window_key(&self) -> WindowKey {
        self.current_window
    }

    pub fn set_current_window(&mut self, window_key: WindowKey) -> anyhow::Result<()> {
        if !self.windows.contains_key(window_key) {
            anyhow::bail!("Window does not exist");
        }
        self.current_window = window_key;
        Ok(())
    }

    pub fn insert_window(&mut self, window: Window) -> anyhow::Result<WindowKey> {
        if !self.files.contains_key(window.file()) {
            anyhow::bail!("File does not exist");
        }
        Ok(self.windows.insert(window))
    }

    // TODO: remove_window

    #[must_use]
    pub fn windows(&self) -> &SlotMap<WindowKey, Window> {
        &self.windows
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

    pub fn assert_valid(&self) {
        for file in self.files.values() {
            file.assert_valid();
        }

        for window in self.windows.values() {
            window.assert_valid();
            assert!(
                self.files.contains_key(window.file()),
                "Window's file is valid"
            );
        }

        assert!(
            self.windows.get(self.current_window).is_some(),
            "Current window key is valid"
        );
    }
}
