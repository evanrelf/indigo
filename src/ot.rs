use ropey::Rope;
use std::borrow::Cow;

enum Operation {
    Retain(usize),
    Delete(usize),
    Insert(String),
}

pub struct Operations {
    operations: Vec<Operation>,
    length_before: usize,
    length_after: usize,
}

impl Operations {
    pub fn new() -> Self {
        Self {
            operations: Vec::new(),
            length_before: 0,
            length_after: 0,
        }
    }

    pub fn from_changes(rope: &Rope, changes: Vec<(usize, usize, Option<&str>)>) -> Self {
        let length = rope.len_chars();
        let mut operations = Self::new();
        let mut last = 0;

        for (from, to, change) in changes {
            assert!(from <= to);
            assert!(last <= from);

            operations.retain(from - last);

            match change {
                Some(s) => {
                    operations.insert(s);
                    operations.delete(to - from);
                }
                None => {
                    operations.delete(to - from);
                }
            }

            last = to;
        }

        operations.retain(length - last);

        operations
    }

    pub fn retain(&mut self, n: usize) {
        if n == 0 {
            return;
        }

        self.length_before += n;
        self.length_after += n;

        if let Some(Operation::Retain(last_n)) = self.operations.last_mut() {
            *last_n += n;
        } else {
            self.operations.push(Operation::Retain(n));
        }
    }

    pub fn delete(&mut self, n: usize) {
        if n == 0 {
            return;
        }

        self.length_before += n;

        if let Some(Operation::Delete(last_n)) = self.operations.last_mut() {
            *last_n += n;
        } else {
            self.operations.push(Operation::Delete(n));
        }
    }

    pub fn insert(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }

        self.length_after += s.chars().count();

        if let Some(Operation::Insert(last_s)) = self.operations.last_mut() {
            *last_s += s;
        } else {
            self.operations.push(Operation::Insert(s.to_string()));
        }
    }

    #[allow(unused_variables)]
    #[allow(unused_mut)]
    pub fn compose(self, other: Self) -> Option<Self> {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn transform(&self, other: &Self) -> Option<Self> {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn transform_index(&self, index: usize) -> usize {
        todo!()
    }

    pub fn apply(&self, rope: &Rope) -> Option<Rope> {
        if rope.len_chars() != self.length_before {
            return None;
        }

        let mut rope = rope.clone();
        let mut position = 0;

        for operation in &self.operations {
            match operation {
                Operation::Retain(n) => {
                    position += n;
                }
                Operation::Delete(n) => {
                    rope.remove(position..position + n);
                }
                Operation::Insert(s) => {
                    rope.insert(position, s);
                    position += s.chars().count();
                }
            }
        }

        Some(rope)
    }

    // Must be called on original rope, before these operations were applied
    pub fn invert(&self, rope: &Rope) -> Option<Self> {
        if rope.len_chars() != self.length_before {
            return None;
        }

        let mut operations = Self::new();
        let mut position = 0;

        for operation in &self.operations {
            match operation {
                Operation::Retain(n) => {
                    operations.retain(*n);
                    position += n;
                }
                Operation::Delete(n) => {
                    let s = Cow::from(rope.slice(position..position + n));
                    operations.insert(s.as_ref());
                    position += n;
                }
                Operation::Insert(s) => {
                    operations.delete(s.chars().count());
                }
            }
        }

        Some(operations)
    }
}

impl Default for Operations {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_operations_apply() {
        let hello_world = Rope::from("Hello, world!");
        let world_to_evan = {
            let mut operations = Operations::new();
            operations.retain(7);
            operations.delete(6);
            operations.insert("Evan!");
            operations
        };
        let hello_evan = world_to_evan.apply(&hello_world).unwrap();
        assert_eq!(hello_evan, "Hello, Evan!");
    }

    #[test]
    fn test_operations_invert() {
        let hello_world_0 = Rope::from("Hello, world!");
        let world_to_evan = {
            let mut operations = Operations::new();
            operations.retain(7);
            operations.delete(6);
            operations.insert("Evan!");
            operations
        };
        let hello_evan = world_to_evan.apply(&hello_world_0).unwrap();
        let evan_to_world = world_to_evan.invert(&hello_world_0).unwrap();
        let hello_world_1 = evan_to_world.apply(&hello_evan).unwrap();
        assert_eq!(hello_world_0, hello_world_1);
    }

    #[test]
    fn test_operations_from_changes() {
        let hello_world = Rope::from("Hello, world!");
        let world_to_evan = Operations::from_changes(&hello_world, vec![(7, 12, Some("Evan"))]);
        let hello_evan = world_to_evan.apply(&hello_world).unwrap();
        assert_eq!(hello_evan, "Hello, Evan!");

        let empty = Rope::from("");
        let empty_to_full = Operations::from_changes(&empty, vec![(0, 0, Some("Full"))]);
        let full = empty_to_full.apply(&empty).unwrap();
        assert_eq!(full, "Full");
    }
}
