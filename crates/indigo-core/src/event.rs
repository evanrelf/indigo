use crate::key::Key;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    KeyInput(Key),
}

impl From<Key> for Event {
    fn from(key: Key) -> Self {
        Self::KeyInput(key)
    }
}
