use slotmap::SlotMap;

pub trait Has<E>
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

    fn get_many_mut<const N: usize>(&mut self, entity_keys: [Self::Key; N]) -> Option<[&mut E; N]> {
        self.has_mut().get_disjoint_mut(entity_keys)
    }

    fn remove(&mut self, entity_key: Self::Key) -> Option<E> {
        self.has_mut().remove(entity_key)
    }
}
