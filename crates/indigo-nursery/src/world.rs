use slotmap::SlotMap;

trait Has<E>
where
    E: Sized,
    Self::Key: slotmap::Key,
{
    type Key;

    fn has(&self) -> &SlotMap<Self::Key, E>;

    fn has_mut(&mut self) -> &mut SlotMap<Self::Key, E>;

    fn insert(&mut self, entity: E) -> Self::Key {
        self.has_mut().insert(entity)
    }

    fn get(&self, entity_key: Self::Key) -> Option<&E> {
        self.has().get(entity_key)
    }

    fn get_mut(&mut self, entity_key: Self::Key) -> Option<&mut E> {
        self.has_mut().get_mut(entity_key)
    }

    fn remove(&mut self, entity_key: Self::Key) -> Option<E> {
        self.has_mut().remove(entity_key)
    }
}

struct Widget;

slotmap::new_key_type! { struct WidgetKey; }

struct World {
    widgets: SlotMap<WidgetKey, Widget>,
}

impl World {
    fn new() -> Self {
        Self {
            widgets: SlotMap::with_key(),
        }
    }
}

impl Has<Widget> for World {
    type Key = WidgetKey;

    fn has(&self) -> &SlotMap<Self::Key, Widget> {
        &self.widgets
    }

    fn has_mut(&mut self) -> &mut SlotMap<Self::Key, Widget> {
        &mut self.widgets
    }
}
