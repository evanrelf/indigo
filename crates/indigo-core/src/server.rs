use crate::{
    buffer::Buffer,
    session::{Action, Event, Session},
};

#[derive(Clone, Default)]
pub struct Server {
    pub session: Session,
}

impl Server {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn handle_event(&mut self, event: Event) -> anyhow::Result<bool> {
        self.session.handle_event(event)
    }

    pub fn handle_action(&mut self, action: &Action) {
        self.session.handle_action(action);
    }
}

impl From<Buffer> for Server {
    fn from(buffer: Buffer) -> Self {
        Self {
            session: Session::from(buffer),
        }
    }
}
