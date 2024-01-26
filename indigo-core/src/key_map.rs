use crate::Key;
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
