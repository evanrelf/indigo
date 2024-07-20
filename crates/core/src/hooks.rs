use crate::{Editor, Event, EventKind};
use std::collections::HashMap;

pub type Hook = fn(&mut Editor, &Event) -> anyhow::Result<()>;

#[derive(Default)]
pub struct Hooks {
    #[allow(clippy::type_complexity)]
    hooks: HashMap<EventKind, Vec<Hook>>,
}

impl Hooks {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn on(&mut self, event_kind: EventKind, hook: Hook) -> &mut Self {
        self.hooks
            .entry(event_kind)
            .and_modify(|hooks| hooks.push(hook))
            .or_insert(vec![hook]);
        self
    }

    pub fn trigger(&self, editor: &mut Editor, event: &Event) -> anyhow::Result<()> {
        if let Some(hooks) = self.hooks.get(&event.kind()) {
            for hook in hooks {
                hook(editor, event)?;
            }
        };

        Ok(())
    }
}
