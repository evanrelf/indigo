use crate::key::{key_c2i, key_i2c};
use anyhow::anyhow;

mod c {
    pub use crossterm::event::Event;
}

mod i {
    pub use indigo_core::event::Event;
}

pub fn event_c2i(event: &c::Event) -> anyhow::Result<i::Event> {
    match event {
        c::Event::Key(key_event) => Ok(i::Event::Key(key_c2i(*key_event)?)),
        _ => Err(anyhow!(
            "Unsupported crossterm->indigo event conversion: {event:?}"
        )),
    }
}

pub fn event_i2c(event: &i::Event) -> c::Event {
    match event {
        i::Event::Key(key_event) => c::Event::Key(key_i2c(*key_event)),
    }
}
