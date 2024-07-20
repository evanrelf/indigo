use crate::key::key_c2i;
use anyhow::anyhow;

mod c {
    pub use crossterm::event::Event;
}

mod i {
    pub use indigo_core::Event;
}

pub fn event_c2i(event: &c::Event) -> anyhow::Result<i::Event> {
    match event {
        c::Event::Key(key_event) => Ok(i::Event::Key(key_c2i(*key_event)?)),
        _ => Err(anyhow!("Unsupported crossterm event: {event:?}")),
    }
}
