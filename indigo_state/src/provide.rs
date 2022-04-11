use crate::type_map::TypeMap;

pub trait Provide {
    fn provide(self, type_map: &mut TypeMap);
}

impl<T> Provide for Box<dyn Fn(&mut T)>
where
    T: 'static,
{
    fn provide(self, type_map: &mut TypeMap) {
        if let Some(t) = type_map.get_mut() {
            self(t);
        } else {
            panic!("Could not provide");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Default, PartialEq)]
    struct Counter(isize);

    fn increment(counter: &mut Counter) {
        counter.0 += 1;
    }

    #[test]
    fn test() {
        let mut type_map = TypeMap::new();
        type_map.insert(Counter::default());
        (Box::new(increment) as Box<dyn Fn(&mut Counter)>).provide(&mut type_map);
        assert_eq!(type_map.remove::<Counter>(), Some(Box::new(Counter(1))));
    }
}
