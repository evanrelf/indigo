use crate::{
    key,
    key::{types, values},
    Key, Reflect,
};
use std::collections::HashMap;

pub struct KeyMap<S, E> {
    #[allow(clippy::type_complexity)]
    mappings: HashMap<values::Key, Box<dyn Fn(&mut S) -> Result<(), E>>>,
}

impl<S, E> KeyMap<S, E> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn on<K>(mut self, handler: impl Fn(&mut S) -> Result<(), E> + 'static) -> Self
    where
        types::Key<K>: Reflect<Value = values::Key>,
    {
        let key = key::<K>();

        assert!(
            !self.mappings.contains_key(&key),
            "Key already registered in KeyMap: {key}"
        );

        self.mappings.insert(key, Box::new(handler));

        self
    }

    pub fn dispatch(&self, state: &mut S, key: &Key) -> Result<bool, E> {
        match self.mappings.get(key) {
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
    use crate::key_types::*;

    #[test]
    #[allow(clippy::unnecessary_wraps)]
    fn test() -> anyhow::Result<()> {
        fn handle_up(count: &mut usize) -> anyhow::Result<()> {
            *count += 1;
            Ok(())
        }

        fn handle_down(count: &mut usize) -> anyhow::Result<()> {
            *count = count.saturating_sub(1);
            Ok(())
        }

        let key_map = KeyMap::new()
            .on::<Up>(handle_up)
            .on::<Down>(handle_down)
            .on::<(Control, Char<'c'>)>(|_| panic!());

        let mut state = 0;

        key_map.dispatch(&mut state, &key::<Up>())?;
        key_map.dispatch(&mut state, &key::<Up>())?;
        key_map.dispatch(&mut state, &key::<Up>())?;

        assert_eq!(state, 3);

        key_map.dispatch(&mut state, &key::<Down>())?;

        assert_eq!(state, 2);

        Ok(())
    }
}
