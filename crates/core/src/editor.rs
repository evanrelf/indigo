use crate::{
    action::handle_action,
    buffer::Buffer,
    event::{Event, handle_event},
    mode::Mode,
};
use std::sync::mpsc;

pub struct Editor {
    event_sender: mpsc::Sender<Event>,
    event_receiver: mpsc::Receiver<Event>,
    pub buffer: Buffer,
    pub mode: Mode,
    pub height: usize,
    pub exit: Option<u8>,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn trigger(&mut self, event: impl Into<Event>) {
        for action in handle_event(self, &event.into()) {
            handle_action(self, action);
        }
    }

    pub fn send(&mut self, event: impl Into<Event>) {
        self.event_sender.send(event.into()).unwrap();
    }

    pub fn flush(&mut self) {
        while let Ok(event) = self.event_receiver.try_recv() {
            self.trigger(event);
        }
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::from(Buffer::new())
    }
}

impl From<Buffer> for Editor {
    fn from(buffer: Buffer) -> Self {
        let (event_sender, event_receiver) = mpsc::channel();
        Self {
            event_sender,
            event_receiver,
            buffer,
            mode: Mode::default(),
            height: 0,
            exit: None,
        }
    }
}
