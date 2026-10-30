use crate::{
    buffer::Buffer,
    client::{Action, Client, ClientMut, ClientState, Event},
    fs::{Fs, NoFs},
    session::Session,
};
use camino::Utf8PathBuf;
use std::sync::Arc;

#[derive(Clone)]
pub struct Server {
    pub fs: Arc<dyn Fs + Send + Sync>,
    pub home: Option<Utf8PathBuf>,
    session: Session,
    client: ClientState,
}

impl Server {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn client(&self) -> Client<'_> {
        Client::new(&self.fs, self.home.as_deref(), &self.session, &self.client)
    }

    pub fn client_mut(&mut self) -> ClientMut<'_> {
        ClientMut::new(
            &self.fs,
            self.home.as_deref(),
            &mut self.session,
            &mut self.client,
        )
    }

    pub fn handle_event(&mut self, event: Event) -> anyhow::Result<bool> {
        self.client_mut().handle_event(event)
    }

    pub fn handle_action(&mut self, action: &Action) {
        self.client_mut().handle_action(action);
    }
}

impl Default for Server {
    fn default() -> Self {
        Self::from(Buffer::default())
    }
}

impl From<Buffer> for Server {
    fn from(buffer: Buffer) -> Self {
        let mut session = Session::new();
        let buffer_key = session.insert_buffer(buffer);
        let client = ClientState::new(buffer_key);
        Self {
            fs: Arc::new(NoFs),
            home: None,
            session,
            client,
        }
    }
}
