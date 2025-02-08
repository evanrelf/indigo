use crate::key::Key;

pub enum Event {
    Key(Key),
}

impl Event {
    #[must_use]
    pub fn kind(&self) -> EventKind {
        match self {
            Self::Key(_) => EventKind::Key,
        }
    }
}

#[derive(Eq, Hash, PartialEq)]
pub enum EventKind {
    Key,
}
