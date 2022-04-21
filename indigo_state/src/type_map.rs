use crate::field::Field;
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
        Self::default()
    }

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

impl<T> Field<T> for TypeMap
where
    T: Any,
{
    type Error = ();

    fn field(&self) -> Result<&T, Self::Error> {
        self.get().ok_or(())
    }

    fn field_mut(&mut self) -> Result<&mut T, Self::Error> {
        self.get_mut().ok_or(())
    }
}

#[macro_export]
macro_rules! type_map {
    () => {
        $crate::type_map::TypeMap::new();
    };
    ($($value:expr),+ $(,)?) => {{
        let mut type_map = $crate::type_map::TypeMap::new();
        $(type_map.insert($value);)*
        type_map
    }};
}

#[cfg(test)]
mod test {
    #[test]
    fn test_type_map() {
        let type_map = type_map!["hello world", 'q', 42u32];
        assert_eq!(type_map.get::<u32>(), Some(&42));
    }
}
