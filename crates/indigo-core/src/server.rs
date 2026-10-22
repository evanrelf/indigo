use crate::{
    buffer::Buffer,
    client::{Client, ClientMut, ClientState},
    session::{Action, Event, Session},
};

#[derive(Clone)]
pub struct Server {
    session: Session,
    client: ClientState,
}

impl Server {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn client(&self) -> Client<'_> {
        Client::new(&self.session, &self.client)
    }

    pub fn client_mut(&mut self) -> ClientMut<'_> {
        ClientMut::new(&mut self.session, &mut self.client)
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
        Self { session, client }
    }
}
