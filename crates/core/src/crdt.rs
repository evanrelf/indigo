use std::collections::HashMap;

// From https://github.com/josephg/crdt-from-scratch + https://www.youtube.com/watch?v=_lQ2Q4Kzi1I.

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ReplicaId(pub usize);

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub struct Sequence(pub usize);

#[derive(Default)]
pub struct Version(pub HashMap<ReplicaId, Sequence>);

impl Version {
    fn contains(&self, item_id: ItemId) -> bool {
        if let Some(highest_sequence) = self.0.get(&item_id.replica_id) {
            *highest_sequence >= item_id.sequence
        } else {
            false
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct ItemId {
    pub replica_id: ReplicaId,
    pub sequence: Sequence,
}

pub struct Item {
    pub id: ItemId,
    pub origin_left: Option<ItemId>,
    pub origin_right: Option<ItemId>,
    pub content: String,
    pub is_deleted: bool,
}

#[derive(Default)]
pub struct Document {
    pub content: Vec<Item>,
    pub version: Version,
}

impl Document {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn content(&self) -> String {
        let mut content = String::new();
        for item in &self.content {
            if !item.is_deleted {
                content.push_str(&item.content);
            }
        }
        content
    }

    fn local_insert_one(&mut self, replica_id: ReplicaId, position: usize, content: char) {
        self.integrate(Item {
            id: ItemId {
                replica_id,
                sequence: match self.version.0.get(&replica_id) {
                    Some(s) => Sequence(s.0 + 1),
                    None => Sequence(0),
                },
            },
            origin_left: if position != 0 {
                self.content.get(position - 1).map(|item| item.id)
            } else {
                None
            },
            origin_right: self.content.get(position).map(|item| item.id),
            content: String::from(content),
            is_deleted: false,
        });
    }

    fn local_insert(&mut self, replica_id: ReplicaId, mut position: usize, content: &str) {
        for char in content.chars() {
            self.local_insert_one(replica_id, position, char);
            position += 1;
        }
    }

    fn remote_insert(&mut self, item: Item) {
        self.integrate(item);
    }

    fn find_index(&self, item_id: ItemId) -> Option<usize> {
        for (i, item) in self.content.iter().enumerate() {
            if item.id == item_id {
                return Some(i);
            }
        }
        None
    }

    fn integrate(&mut self, item: Item) {
        if let Some(last_seen) = self.version.0.get(&item.id.replica_id) {
            assert_eq!(
                item.id.sequence.0,
                last_seen.0 + 1,
                "Operations out of order"
            );
        }
        self.version.0.insert(item.id.replica_id, item.id.sequence);
        let left = item.origin_left.and_then(|id| self.find_index(id));
        let mut dest_idx = match left {
            Some(left) => left + 1,
            None => 0,
        };
        let right = item
            .origin_right
            .and_then(|id| self.find_index(id))
            .unwrap_or(self.content.len());
        let mut scanning = false;
        #[expect(clippy::mut_range_bound)]
        for i in dest_idx..=self.content.len() {
            if !scanning {
                dest_idx = i;
            }
            if i == right {
                break;
            }
            let other = &self.content[i];
            let other_left = other.origin_left.and_then(|id| self.find_index(id));
            let other_right = other
                .origin_right
                .and_then(|id| self.find_index(id))
                .unwrap_or(self.content.len());
            if other_left < left
                || (other_left == left
                    && other_right == right
                    && item.id.replica_id == other.id.replica_id)
            {
                break;
            }
            if other_left == Some(other_right) {
                scanning = other_right < right;
            }
        }
        self.content.insert(dest_idx, item);
    }

    fn can_insert_now(&self, item: &Item) -> bool {
        let not_in_version = !self.version.contains(item.id);
        let left_in_version = item.origin_left.is_none_or(|id| self.version.contains(id));
        let right_in_version = item.origin_right.is_none_or(|id| self.version.contains(id));
        not_in_version && left_in_version && right_in_version
    }

    fn merge(&mut self, other: &Self) {
        let missing_items = other
            .content
            .iter()
            .filter(|item| !self.version.contains(item.id))
            .collect::<Vec<_>>();
        let mut missing_items_count = missing_items.len();
        while missing_items_count > 0 {
            let mut merged_this_loop = 0;
            for (i, item) in missing_items.iter().enumerate() {
                // 58:00 in https://www.youtube.com/watch?v=_lQ2Q4Kzi1I
                todo!()
            }
            assert!(merged_this_loop > 0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(replica_id: usize, sequence: usize) -> ItemId {
        ItemId {
            replica_id: ReplicaId(replica_id),
            sequence: Sequence(sequence),
        }
    }

    #[test]
    fn test() {
        let mut document = Document::new();
        document.local_insert_one(ReplicaId(0), 0, 'a');
        document.local_insert_one(ReplicaId(0), 1, 'b');
        document.local_insert_one(ReplicaId(0), 0, 'c');
        assert_eq!(document.content(), "cab");
    }
}
