use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Default)]
pub struct TypeMap {
    hash_map: HashMap<TypeId, Box<dyn Any>>,
}

impl TypeMap {
    pub fn new() -> Self {
        TypeMap::default()
    }

    pub fn insert<T>(&mut self, value: T) -> Option<Box<T>>
    where
        T: 'static,
    {
        self.hash_map
            .insert(value.type_id(), Box::new(value))
            .and_then(|b| b.downcast().ok())
    }

    pub fn get<T>(&self) -> Option<&T>
    where
        T: 'static,
    {
        self.hash_map
            .get(&TypeId::of::<T>())
            .and_then(|b| b.downcast_ref())
    }

    pub fn get_mut<T>(&mut self) -> Option<&mut T>
    where
        T: 'static,
    {
        self.hash_map
            .get_mut(&TypeId::of::<T>())
            .and_then(|b| b.downcast_mut())
    }

    pub fn remove<T>(&mut self) -> Option<Box<T>>
    where
        T: 'static,
    {
        self.hash_map
            .remove(&TypeId::of::<T>())
            .and_then(|b| b.downcast().ok())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut type_map = TypeMap::new();
        type_map.insert(42u32);
        type_map.insert(true);
        type_map.insert(String::default());
        assert_eq!(type_map.remove::<u32>(), Some(Box::new(42)));
        assert_eq!(type_map.remove::<bool>(), Some(Box::new(true)));
        assert_eq!(type_map.remove::<String>(), Some(Box::new("".to_string())));
        assert_eq!(type_map.remove::<i32>(), None);
    }
}
