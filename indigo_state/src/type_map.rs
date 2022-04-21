use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Default)]
pub struct TypeMap {
    hash_map: HashMap<TypeId, Box<dyn Any>>,
}

impl TypeMap {
    pub fn insert<T>(&mut self, value: T)
    where
        T: Any,
    {
        self.hash_map.insert(value.type_id(), Box::new(value));
    }

    pub fn get<T>(&self) -> Option<&T>
    where
        T: Any,
    {
        self.hash_map
            .get(&TypeId::of::<T>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_ref().unwrap())
    }

    pub fn get_mut<T>(&mut self) -> Option<&mut T>
    where
        T: Any,
    {
        self.hash_map
            .get_mut(&TypeId::of::<T>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_mut().unwrap())
    }
}
