use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, ops::RangeBounds};

pub struct Buffer {
    text: Rope,
    crdt: cola::Replica,
}

impl Buffer {
    #[must_use]
    pub fn new<T>(id: u64, text: T) -> Self
    where
        T: Into<Rope>,
    {
        let text = text.into();
        let crdt = cola::Replica::new(id, text.len_chars());
        Self { text, crdt }
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn insert<T>(&mut self, index: usize, text: T) -> Insertion
    where
        T: Into<String>,
    {
        let text = text.into();
        self.text.insert(index, &text);
        let crdt = self.crdt.inserted(index, text.len());
        Insertion { text, crdt }
    }

    #[must_use]
    pub fn delete<R>(&mut self, range: R) -> Deletion
    where
        R: RangeBounds<usize> + Clone,
    {
        self.text.remove(range.clone());
        Deletion(self.crdt.deleted(range))
    }

    #[must_use]
    pub fn create_anchor(&self, index: usize) -> Anchor {
        Anchor(self.crdt.create_anchor(index, cola::AnchorBias::Right))
    }

    #[must_use]
    pub fn resolve_anchor(&self, anchor: Anchor) -> Option<usize> {
        self.crdt.resolve_anchor(anchor.0)
    }

    #[must_use]
    pub fn fork(&self, new_id: u64) -> Self {
        let text = self.text.clone();
        let crdt = self.crdt.fork(new_id);
        Self { text, crdt }
    }

    #[must_use]
    pub fn encode(&self) -> EncodedBuffer {
        let text = Cow::<'_, str>::from(self.text.clone());
        let crdt = self.crdt.encode();
        EncodedBuffer { text, crdt }
    }

    pub fn decode(id: u64, encoded: EncodedBuffer) -> Result<Self, cola::DecodeError> {
        let text = Rope::from(encoded.text);
        let crdt = cola::Replica::decode(id, &encoded.crdt)?;
        Ok(Self { text, crdt })
    }

    pub fn integrate_insertion(&mut self, insertion: &Insertion) {
        if let Some(offset) = self.crdt.integrate_insertion(&insertion.crdt) {
            self.text.insert(offset, &insertion.text);
        }
    }

    pub fn integrate_deletion(&mut self, deletion: &Deletion) {
        let ranges = self.crdt.integrate_deletion(&deletion.0);
        for range in ranges.into_iter().rev() {
            self.text.remove(range);
        }
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct Insertion {
    text: String,
    crdt: cola::Insertion,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Deletion(cola::Deletion);

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
pub struct Anchor(cola::Anchor);

#[derive(Clone, Deserialize, Serialize)]
pub struct EncodedBuffer<'rope> {
    #[serde(borrow)]
    text: Cow<'rope, str>,
    crdt: cola::EncodedReplica,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut peer_1 = Buffer::new(1, "Hello, world");
        let mut peer_2 = peer_1.fork(2);

        let delete_comma = peer_1.delete(5..6);
        let insert_exclamation = peer_2.insert(12, "!");

        peer_1.integrate_insertion(&insert_exclamation);
        peer_2.integrate_deletion(&delete_comma);

        assert_eq!(peer_1.text, "Hello world!");
        assert_eq!(peer_2.text, "Hello world!");
    }
}
