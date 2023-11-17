use crate::{window::WindowState, File, FileKey, Mode, Window, WindowKey, WindowMut};
use slotmap::SlotMap;

#[derive(Debug)]
pub struct Editor {
    files: SlotMap<FileKey, File>,
    windows: SlotMap<WindowKey, WindowState>,
    current_window: WindowKey,
    mode: Mode,
}

impl Editor {
    #[must_use]
    pub fn new(file: File) -> Self {
        let mut files = SlotMap::with_key();
        let mut windows = SlotMap::with_key();
        let file_key = files.insert(file);
        let window_key = windows.insert(WindowState::new(file_key));
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
        self.get_file(self.current_file_key()).unwrap()
    }

    #[must_use]
    pub fn current_file_mut(&mut self) -> &mut File {
        self.get_file_mut(self.current_file_key()).unwrap()
    }

    #[must_use]
    pub fn current_file_key(&self) -> FileKey {
        self.windows[self.current_window].file_key()
    }

    pub fn insert_file(&mut self, file: File) -> FileKey {
        self.files.insert(file)
    }

    // TODO: remove_file

    pub fn files(&self) -> impl Iterator<Item = &File> + '_ {
        self.files.values()
    }

    pub fn files_mut(&mut self) -> impl Iterator<Item = &mut File> + '_ {
        self.files.values_mut()
    }

    pub fn file_keys(&self) -> impl Iterator<Item = FileKey> + '_ {
        self.files.keys()
    }

    #[must_use]
    pub fn get_window(&self, window_key: WindowKey) -> Option<Window> {
        let state = self.windows.get(window_key)?;
        let file = &self.files[state.file_key()];
        Some(Window::new(file, state))
    }

    #[must_use]
    pub fn get_window_mut(&mut self, window_key: WindowKey) -> Option<WindowMut> {
        let state = self.windows.get_mut(window_key)?;
        let file = &mut self.files[state.file_key()];
        Some(WindowMut::new(file, state))
    }

    #[must_use]
    pub fn current_window(&self) -> Window {
        self.get_window(self.current_window).unwrap()
    }

    #[must_use]
    pub fn current_window_mut(&mut self) -> WindowMut {
        self.get_window_mut(self.current_window).unwrap()
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

    pub fn insert_window(&mut self, file_key: FileKey) -> anyhow::Result<WindowKey> {
        if !self.files.contains_key(file_key) {
            anyhow::bail!("File does not exist");
        }
        Ok(self.windows.insert(WindowState::new(file_key)))
    }

    // TODO: remove_window

    // TODO
    // pub fn windows(&self) -> impl Iterator<Item = &Window> + '_ {
    //     self.windows.values()
    // }

    // TODO
    // pub fn windows_mut(&self) -> impl Iterator<Item = &mut Window> + '_ {
    //     self.windows.values_mut()
    // }

    pub fn window_keys(&self) -> impl Iterator<Item = WindowKey> + '_ {
        self.windows.keys()
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

        for window_state in self.windows.values() {
            assert!(
                self.files.contains_key(window_state.file_key()),
                "Window's file is valid"
            );
        }

        assert!(
            self.windows.get(self.current_window).is_some(),
            "Current window key is valid"
        );
    }
}
