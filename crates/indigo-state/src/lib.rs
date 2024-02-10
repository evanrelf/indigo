use cola::{Anchor, Deletion, EncodedReplica, Replica, ReplicaId};
use ropey::Rope;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, ops::RangeBounds};

pub struct Buffer {
    text: Rope,
    crdt: Replica,
}

impl Buffer {
    #[must_use]
    pub fn new<T>(replica_id: ReplicaId, text: T) -> Self
    where
        T: Into<Rope>,
    {
        let text = text.into();
        let crdt = Replica::new(replica_id, text.len_chars());
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
        self.crdt.deleted(range)
    }

    #[must_use]
    pub fn create_anchor(&self, index: usize) -> Anchor {
        self.crdt.create_anchor(index, cola::AnchorBias::Right)
    }

    #[must_use]
    pub fn resolve_anchor(&self, anchor: Anchor) -> Option<usize> {
        self.crdt.resolve_anchor(anchor)
    }

    #[must_use]
    pub fn fork(&self, new_replica_id: ReplicaId) -> Self {
        let text = self.text.clone();
        let crdt = self.crdt.fork(new_replica_id);
        Self { text, crdt }
    }

    #[must_use]
    pub fn encode(&self) -> EncodedBuffer {
        let text = Cow::<'_, str>::from(self.text.clone());
        let crdt = self.crdt.encode();
        EncodedBuffer { text, crdt }
    }

    pub fn decode(
        replica_id: ReplicaId,
        encoded: EncodedBuffer,
    ) -> Result<Self, cola::DecodeError> {
        let text = Rope::from(encoded.text);
        let crdt = Replica::decode(replica_id, &encoded.crdt)?;
        Ok(Self { text, crdt })
    }

    pub fn integrate_insertion(&mut self, insertion: &Insertion) {
        if let Some(offset) = self.crdt.integrate_insertion(&insertion.crdt) {
            self.text.insert(offset, &insertion.text);
        }
    }

    pub fn integrate_deletion(&mut self, deletion: &Deletion) {
        let ranges = self.crdt.integrate_deletion(deletion);
        for range in ranges.into_iter().rev() {
            self.text.remove(range);
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct EncodedBuffer<'rope> {
    #[serde(borrow)]
    text: Cow<'rope, str>,
    crdt: EncodedReplica,
}

#[derive(Deserialize, Serialize)]
pub struct Insertion {
    text: String,
    crdt: cola::Insertion,
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
