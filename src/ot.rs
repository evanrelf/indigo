use ropey::Rope;
use std::borrow::Cow;

enum Operation {
    Retain(usize),
    Delete(usize),
    Insert(String),
}

pub(crate) struct Operations {
    operations: Vec<Operation>,
    length_before: usize,
    length_after: usize,
}

impl Operations {
    pub(crate) fn new() -> Self {
        Self {
            operations: Vec::new(),
            length_before: 0,
            length_after: 0,
        }
    }

    pub(crate) fn retain(mut self, n: usize) -> Self {
        self.operations.push(Operation::Retain(n));
        self.length_before += n;
        self.length_after += n;
        self
    }

    pub(crate) fn delete(mut self, n: usize) -> Self {
        self.operations.push(Operation::Delete(n));
        self.length_before += n;
        self
    }

    pub(crate) fn insert(mut self, s: &str) -> Self {
        self.operations.push(Operation::Insert(s.to_string()));
        self.length_after += s.chars().count();
        self
    }

    pub(crate) fn compose(mut self, other: Self) -> Option<Self> {
        todo!()
    }

    pub(crate) fn transform(mut self, other: Self) -> Option<Self> {
        todo!()
    }

    pub(crate) fn transform_index(&self, index: usize) -> usize {
        todo!()
    }

    pub(crate) fn apply(&self, rope: &Rope) -> Option<Rope> {
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
    pub(crate) fn invert(&self, rope: &Rope) -> Option<Self> {
        if rope.len_chars() != self.length_before {
            return None;
        }

        let mut operations = Self::new();
        let mut position = 0;

        for operation in &self.operations {
            match operation {
                Operation::Retain(n) => {
                    operations = operations.retain(*n);
                    position += n;
                }
                Operation::Delete(n) => {
                    let s = Cow::from(rope.slice(position..position + n));
                    operations = operations.insert(s.as_ref());
                    position += n;
                }
                Operation::Insert(s) => {
                    operations = operations.delete(s.chars().count());
                }
            }
        }

        Some(operations)
    }
}

#[test]
fn test_operations_apply() {
    let hello_world = Rope::from("Hello, world!");
    let world_to_evan = Operations::new().retain(7).delete(6).insert("Evan!");
    let hello_evan = world_to_evan.apply(&hello_world).unwrap();
    assert_eq!(hello_evan, "Hello, Evan!");
}

#[test]
fn test_operations_invert() {
    let hello_world_0 = Rope::from("Hello, world!");
    let world_to_evan = Operations::new().retain(7).delete(6).insert("Evan!");
    let hello_evan = world_to_evan.apply(&hello_world_0).unwrap();
    let evan_to_world = world_to_evan.invert(&hello_world_0).unwrap();
    let hello_world_1 = evan_to_world.apply(&hello_evan).unwrap();
    assert_eq!(hello_world_0, hello_world_1);
}
