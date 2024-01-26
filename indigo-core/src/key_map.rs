use crate::{Editor, Key};
use once_cell::sync::Lazy;
use std::collections::HashMap;

pub struct KeyMap<S, E> {
    #[allow(clippy::type_complexity)]
    mappings: HashMap<Key, Box<dyn Fn(&mut S) -> Result<(), E>>>,
}

impl<S, E> KeyMap<S, E> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn on<K>(mut self, key: K, handler: impl Fn(&mut S) -> Result<(), E> + 'static) -> Self
    where
        K: Into<Key>,
    {
        let key = key.into();

        assert!(
            !self.mappings.contains_key(&key),
            "Key already registered in KeyMap: {key}"
        );

        self.mappings.insert(key, Box::new(handler));

        self
    }

    pub fn dispatch<K>(&self, state: &mut S, key: K) -> Result<bool, E>
    where
        K: Into<Key>,
    {
        match self.mappings.get(&key.into()) {
            None => Ok(false),
            Some(handler) => handler(state).map(|()| true),
        }
    }
}

impl<S, E> Default for KeyMap<S, E> {
    fn default() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }
}

struct Mapping {
    key: Key,
    handler: fn(&mut Editor) -> anyhow::Result<()>,
}

#[linkme::distributed_slice]
static MAPPINGS: [Lazy<Mapping>];

pub fn global_mappings() -> KeyMap<Editor, anyhow::Error> {
    let mut key_map = KeyMap::new();
    for mapping in MAPPINGS {
        key_map = key_map.on(mapping.key, mapping.handler);
    }
    key_map
}

macro_rules! on {
    ($key:expr, $handler:ident) => {
        #[::linkme::distributed_slice($crate::key_map::MAPPINGS)]
        static _MAPPING: ::once_cell::sync::Lazy<$crate::key_map::Mapping> =
            ::once_cell::sync::Lazy::new(|| {
                #[allow(unused_imports)]
                use $crate::{Key, KeyCode::*, KeyModifier::*};
                $crate::key_map::Mapping {
                    key: Key::from($key),
                    handler: $handler,
                }
            });
    };
}

pub(crate) use on;

// TODO: Attribute proc macro
// on!((Control, 'c'), test);
// fn test(_editor: &mut Editor) -> anyhow::Result<()> {
//     panic!()
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::unnecessary_wraps)]
    fn test() -> anyhow::Result<()> {
        use crate::{KeyCode::*, KeyModifier::*};

        fn handle_up(count: &mut usize) -> anyhow::Result<()> {
            *count += 1;
            Ok(())
        }

        fn handle_down(count: &mut usize) -> anyhow::Result<()> {
            *count = count.saturating_sub(1);
            Ok(())
        }

        let key_map = KeyMap::new()
            .on(Up, handle_up)
            .on(Down, handle_down)
            .on((Control, 'c'), |_| panic!());

        let mut state = 0;

        key_map.dispatch(&mut state, Up)?;
        key_map.dispatch(&mut state, Up)?;
        key_map.dispatch(&mut state, Up)?;

        assert_eq!(state, 3);

        key_map.dispatch(&mut state, Down)?;

        assert_eq!(state, 2);

        Ok(())
    }
}
