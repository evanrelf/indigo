use crate::{
    buffer::{Buffer, BufferKey},
    fs::Fs,
    key::Key,
    mode::{Mode, command, insert, normal, prompt, replace, seek},
    session::Session,
    window::{Window, WindowKey, WindowMut, WindowState},
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use slotmap::SlotMap;
use std::{
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    process::ExitCode,
    sync::Arc,
};
use thiserror::Error;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Focused window {key:?} does not exist")]
    FocusedWindowMissing { key: WindowKey },

    #[error("Window {window_key:?} refers to buffer {buffer_key:?}, which does not exist")]
    WindowBufferMissing {
        window_key: WindowKey,
        buffer_key: BufferKey,
    },

    #[error("Error from window {key:?}")]
    Window {
        key: WindowKey,
        #[source]
        source: anyhow::Error,
    },

    #[error("Error from prompt")]
    Prompt(#[source] anyhow::Error),
}

#[derive(Clone)]
pub struct ClientState {
    windows: SlotMap<WindowKey, WindowState>,
    focused_window: WindowKey,
    pub mode: Mode,
    pub count: Option<NonZeroUsize>,
    pub pending_keys: Vec<Key>,
    pub message: Option<Result<String, String>>,
    pub exit_code: Option<ExitCode>,
}

impl ClientState {
    #[must_use]
    pub(crate) fn new(buffer_key: BufferKey) -> Self {
        let window = WindowState::new(buffer_key);
        let mut windows = SlotMap::default();
        let window_key = windows.insert(window);

        Self {
            windows,
            focused_window: window_key,
            mode: Mode::default(),
            count: None,
            pending_keys: Vec::new(),
            message: None,
            exit_code: None,
        }
    }

    /// Doesn't actually terminate the process, just records the desired exit code.
    pub fn exit(&mut self, exit_code: u8) {
        self.exit_code = Some(ExitCode::from(exit_code));
    }
}

#[must_use]
pub struct ClientView<'a, W: Wrap> {
    state: W::Wrap<'a, ClientState>,
    session: W::Wrap<'a, Session>,
    fs: &'a Arc<dyn Fs + Send + Sync>,
}

pub type Client<'a> = ClientView<'a, WRef>;

pub type ClientMut<'a> = ClientView<'a, WMut>;

impl<W: WrapRef> Deref for ClientView<'_, W> {
    type Target = ClientState;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<W: WrapMut> DerefMut for ClientView<'_, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<'a, W: WrapRef> ClientView<'a, W> {
    pub fn new(
        fs: &'a Arc<dyn Fs + Send + Sync>,
        session: W::WrapRef<'a, Session>,
        state: W::WrapRef<'a, ClientState>,
    ) -> Self {
        Self { state, session, fs }
    }

    #[must_use]
    pub fn session(&self) -> &Session {
        &self.session
    }

    #[must_use]
    pub fn fs(&self) -> &'a Arc<dyn Fs + Send + Sync> {
        self.fs
    }

    #[must_use]
    pub fn focused_buffer(&self) -> &Buffer {
        let window = self
            .state
            .windows
            .get(self.state.focused_window)
            .expect("Window state is always kept valid");
        self.session
            .get_buffer(window.buffer)
            .expect("Window state is always kept valid")
    }

    #[must_use]
    pub fn get_window(&self, window_key: WindowKey) -> Option<Window<'_>> {
        let window = self.state.windows.get(window_key)?;
        let buffer = self
            .session
            .get_buffer(window.buffer)
            .expect("Window state is always kept valid");
        Some(Window::new(buffer, window))
    }

    pub fn focused_window(&self) -> Window<'_> {
        let window = self
            .state
            .windows
            .get(self.state.focused_window)
            .expect("Window state is always kept valid");
        let buffer = self
            .session
            .get_buffer(window.buffer)
            .expect("Window state is always kept valid");
        Window::new(buffer, window)
    }

    // TODO: Add windows iterator

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        self.session.assert_invariants()?;
        if !self.state.windows.contains_key(self.state.focused_window) {
            anyhow::bail!(Error::FocusedWindowMissing {
                key: self.state.focused_window,
            });
        }
        for (window_key, window) in &self.state.windows {
            let Some(buffer) = self.session.get_buffer(window.buffer) else {
                anyhow::bail!(Error::WindowBufferMissing {
                    window_key,
                    buffer_key: window.buffer,
                });
            };
            Window::new(buffer, window)
                .assert_invariants()
                .map_err(|source| Error::Window {
                    key: window_key,
                    source,
                })?;
        }
        if let Mode::Prompt(state) = &self.state.mode {
            state.assert_invariants().map_err(Error::Prompt)?;
        }
        Ok(())
    }
}

impl<W: WrapMut> ClientView<'_, W> {
    #[must_use]
    pub fn session_mut(&mut self) -> &mut Session {
        &mut self.session
    }

    #[must_use]
    pub fn focused_buffer_mut(&mut self) -> &mut Buffer {
        let window = self
            .state
            .windows
            .get(self.state.focused_window)
            .expect("Window state is always kept valid");
        self.session
            .get_buffer_mut(window.buffer)
            .expect("Window state is always kept valid")
    }

    // TODO: Figure out a safer/better way to add and remove windows
    pub(crate) fn insert_window(&mut self, window: WindowState) -> WindowKey {
        assert!(
            self.session.get_buffer(window.buffer).is_some(),
            "Window has invalid buffer key"
        );
        self.state.windows.insert(window)
    }

    pub fn focus_window(&mut self, window_key: WindowKey) -> bool {
        if self.state.windows.contains_key(window_key) {
            self.state.focused_window = window_key;
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn get_window_mut(&mut self, window_key: WindowKey) -> Option<WindowMut<'_>> {
        let window = self.state.windows.get_mut(window_key)?;
        let buffer = self
            .session
            .get_buffer_mut(window.buffer)
            .expect("Window state is always kept valid");
        Some(WindowMut::new(buffer, window))
    }

    pub fn focused_window_mut(&mut self) -> WindowMut<'_> {
        let focused_window = self.state.focused_window;
        let window = self
            .state
            .windows
            .get_mut(focused_window)
            .expect("Window state is always kept valid");
        let buffer = self
            .session
            .get_buffer_mut(window.buffer)
            .expect("Window state is always kept valid");
        WindowMut::new(buffer, window)
    }
}

impl ClientMut<'_> {
    pub fn handle_event(&mut self, mut event: Event) -> anyhow::Result<bool> {
        #[expect(irrefutable_let_patterns)]
        if let Event::Key(KeyEvent { key, .. }) = &mut event {
            key.normalize();
            self.state.pending_keys.push(*key);
        }

        let handled = match event {
            Event::Key(_) => match self.state.mode {
                Mode::Normal => normal::handle_keys(self),
                Mode::Seek(_) => seek::handle_keys(self),
                Mode::Insert => insert::handle_keys(self),
                Mode::Prompt(_) => prompt::handle_keys(self),
                Mode::Replace => replace::handle_keys(self),
            },
        };

        self.assert_invariants().unwrap();

        Ok(handled)
    }

    pub fn handle_action(&mut self, action: &Action) {
        match action {
            Action::Normal(action) => normal::handle_action(self, action),
            Action::Insert(action) => insert::handle_action(self, action),
            Action::Prompt(action) => prompt::handle_action(self, action),
            Action::Command(action) => command::handle_action(self, action),
            Action::Seek(action) => seek::handle_action(self, action),
        }
        self.assert_invariants().unwrap();
    }
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    Key(KeyEvent),
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub struct KeyEvent {
    pub key: Key,
    pub kind: KeyEventKind,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub enum KeyEventKind {
    Press,
    Repeat,
    Release,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Normal(normal::Action),
    Insert(insert::Action),
    Prompt(prompt::Action),
    Command(command::Action),
    // TODO: Replace this with multi-key mappings in normal mode
    Seek(seek::Action),
}
