// TODO: Remove
#![allow(clippy::needless_pass_by_value)]
#![allow(unused_variables)]

use arrayvec::ArrayVec;
use std::{cmp::Ordering, sync::Arc};

pub trait Key: Clone + Ord {}

pub trait Value: Clone {}

#[derive(Clone)]
pub struct BTree<K: Key, V: Value> {
    root: Arc<Node<K, V>>,
}

impl<K: Key, V: Value> BTree<K, V> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            root: Arc::new(Node::Leaf(NodeLeaf {
                keys: ArrayVec::new(),
                values: ArrayVec::new(),
            })),
        }
    }

    pub fn insert(&mut self, key: &K, value: V) {
        self.assert_invariants();

        Arc::make_mut(&mut self.root).insert(key, value);
    }

    pub fn append(&mut self, tree: Self) {
        self.assert_invariants();

        todo!()
    }

    #[must_use]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.assert_invariants();

        self.root.get_value(key)
    }

    #[must_use]
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.assert_invariants();

        Arc::make_mut(&mut self.root).get_value_mut(key)
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.assert_invariants();

        Arc::make_mut(&mut self.root).remove(key)
    }

    fn assert_invariants(&self) {
        // TODO: Check ordering of keys
        // TODO: Check children's keys are ordered correctly relative to parent
        // TODO: Check all leaves at same depth / all paths from root same length
        self.root.assert_invariants(true);
    }
}

impl<K: Key, V: Value> Default for BTree<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

const BASE: usize = 4;

#[derive(Clone)]
enum Node<K: Key, V: Value> {
    Branch(NodeBranch<K, V>),
    Leaf(NodeLeaf<K, V>),
}

impl<K: Key, V: Value> Node<K, V> {
    fn insert(&mut self, key: &K, value: V) {
        if let Self::Branch(ref mut branch) = self {
            branch.insert(key, value);
            return;
        }

        // `NodeBranch` takes up slightly less space than `NodeLeaf`, because its `keys` `ArrayVec`
        // is smaller, so we use it as the temporary placeholder.
        let this = std::mem::replace(self, Self::Branch(NodeBranch::new()));

        if let Self::Leaf(leaf) = this {
            *self = leaf.insert(key, value);
            return;
        }

        unreachable!();
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        match self {
            Self::Branch(branch) => branch.remove(key),
            Self::Leaf(leaf) => leaf.remove(key),
        }
    }

    #[must_use]
    fn search(&self, key: &K) -> (Vec<usize>, NodeLeafSearchResult) {
        let mut path = Vec::new();
        let mut node = self;

        loop {
            match node {
                Self::Branch(branch) => {
                    let i = branch.search(key);
                    path.push(i);
                    node = branch.trees[i].root.as_ref();
                }
                Self::Leaf(leaf) => return (path, leaf.search(key)),
            }
        }
    }

    #[must_use]
    fn get_leaf(&self, key: &K) -> &NodeLeaf<K, V> {
        match self {
            Self::Branch(branch) => {
                let next = &branch.get(key).root;
                next.get_leaf(key)
            }
            Self::Leaf(leaf) => leaf,
        }
    }

    #[must_use]
    fn get_leaf_mut(&mut self, key: &K) -> &mut NodeLeaf<K, V> {
        match self {
            Self::Branch(branch) => {
                let next = Arc::make_mut(&mut branch.get_mut(key).root);
                next.get_leaf_mut(key)
            }
            Self::Leaf(leaf) => leaf,
        }
    }

    #[must_use]
    fn get_value(&self, key: &K) -> Option<&V> {
        let NodeLeaf { keys, values } = self.get_leaf(key);

        for (i, k) in keys.iter().enumerate() {
            match key.cmp(k) {
                Ordering::Less => continue,
                Ordering::Equal => return Some(&values[i]),
                Ordering::Greater => break,
            }
        }

        None
    }

    #[must_use]
    fn get_value_mut(&mut self, key: &K) -> Option<&mut V> {
        let NodeLeaf { keys, values } = self.get_leaf_mut(key);

        for (i, k) in keys.iter().enumerate() {
            match key.cmp(k) {
                Ordering::Less => continue,
                Ordering::Equal => return Some(&mut values[i]),
                Ordering::Greater => break,
            }
        }

        None
    }

    #[must_use]
    fn is_empty(&self) -> bool {
        match self {
            Self::Branch(branch) => branch.is_empty(),
            Self::Leaf(leaf) => leaf.is_empty(),
        }
    }

    #[must_use]
    fn is_full(&self) -> bool {
        match self {
            Self::Branch(branch) => branch.is_full(),
            Self::Leaf(leaf) => leaf.is_full(),
        }
    }

    fn assert_invariants(&self, is_root: bool) {
        match self {
            Self::Branch(branch) => branch.assert_invariants(is_root),
            Self::Leaf(leaf) => leaf.assert_invariants(is_root),
        }
    }
}

#[derive(Clone)]
struct NodeBranch<K: Key, V: Value> {
    keys: ArrayVec<K, { BASE - 1 }>,
    trees: ArrayVec<BTree<K, V>, BASE>,
}

impl<K: Key, V: Value> NodeBranch<K, V> {
    fn new() -> Self {
        Self {
            keys: ArrayVec::new(),
            trees: ArrayVec::new(),
        }
    }

    fn insert(&mut self, key: &K, value: V) {
        todo!()
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        let i = self.search(key);
        let tree = &mut self.trees[i];
        let value = tree.remove(key);
        if tree.root.is_empty() {
            self.trees.remove(i);
        }
        value
    }

    #[must_use]
    fn search(&self, key: &K) -> usize {
        self.keys
            .iter()
            .enumerate()
            .filter(|(_, k)| *k < key)
            // .max_by_key(|(_, k)| *k)
            .last()
            .map_or(0, |(i, _)| i)
    }

    #[must_use]
    fn get(&self, key: &K) -> &BTree<K, V> {
        let i = self.search(key);
        &self.trees[i]
    }

    #[must_use]
    fn get_mut(&mut self, key: &K) -> &mut BTree<K, V> {
        let i = self.search(key);
        &mut self.trees[i]
    }

    #[must_use]
    fn is_empty(&self) -> bool {
        match (self.keys.is_empty(), self.trees.is_empty()) {
            (true, true) => true,
            (false, false) => false,
            _ => unreachable!(),
        }
    }

    #[must_use]
    fn is_full(&self) -> bool {
        match (self.keys.is_full(), self.trees.is_full()) {
            (true, true) => true,
            (false, false) => false,
            _ => unreachable!(),
        }
    }

    fn assert_invariants(&self, is_root: bool) {
        debug_assert!(
            self.keys.len() + 1 == self.trees.len(),
            "Invalid number of self.keys relative to self.trees"
        );
        if is_root {
            debug_assert!(
                self.trees.len() >= 2,
                "Root must have at least two children when branch"
            );
        } else {
            debug_assert!(
                self.keys.len() >= (self.keys.capacity() / 2) + (self.keys.capacity() % 2),
                "Keys is not half full"
            );
            debug_assert!(
                self.trees.len() >= (self.trees.capacity() / 2) + (self.trees.capacity() % 2),
                "Trees is not half full"
            );
        }
        for tree in &self.trees {
            tree.root.assert_invariants(false);
        }
    }
}

#[derive(Clone)]
struct NodeLeaf<K: Key, V: Value> {
    keys: ArrayVec<K, BASE>,
    values: ArrayVec<V, BASE>,
}

impl<K: Key, V: Value> NodeLeaf<K, V> {
    fn new() -> Self {
        Self {
            keys: ArrayVec::new(),
            values: ArrayVec::new(),
        }
    }

    fn insert(mut self, key: &K, value: V) -> Node<K, V> {
        match self.insert_no_split(key, value) {
            Ok(()) => Node::Leaf(self),
            Err(value) => {
                let (l, k, r) = self.insert_split(key, value);

                let mut keys = ArrayVec::new();
                keys.push(k);

                let mut trees = ArrayVec::new();
                trees.push(BTree {
                    root: Arc::new(Node::Leaf(l)),
                });
                trees.push(BTree {
                    root: Arc::new(Node::Leaf(r)),
                });

                Node::Branch(NodeBranch { keys, trees })
            }
        }
    }

    fn insert_no_split(&mut self, key: &K, value: V) -> Result<(), V> {
        match self.search(key) {
            NodeLeafSearchResult::Found(i) => {
                self.keys[i] = key.clone();
                self.values[i] = value;
                Ok(())
            }
            NodeLeafSearchResult::CanInsert(i) => {
                self.keys.insert(i, key.clone());
                self.values.insert(i, value);
                Ok(())
            }
            NodeLeafSearchResult::CannotInsert(_) => Err(value),
        }
    }

    fn insert_split(self, key: &K, value: V) -> (Self, K, Self) {
        todo!()
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        if let NodeLeafSearchResult::Found(i) = self.search(key) {
            self.keys.remove(i);
            Some(self.values.remove(i))
        } else {
            None
        }
    }

    #[must_use]
    fn search(&self, key: &K) -> NodeLeafSearchResult {
        match self.keys.binary_search(key) {
            Err(i) if self.is_full() => NodeLeafSearchResult::CannotInsert(i),
            Err(i) => NodeLeafSearchResult::CanInsert(i),
            Ok(i) => NodeLeafSearchResult::Found(i),
        }
    }

    #[must_use]
    fn get(&self, key: &K) -> Option<&V> {
        if let NodeLeafSearchResult::Found(i) = self.search(key) {
            Some(&self.values[i])
        } else {
            None
        }
    }

    #[must_use]
    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        if let NodeLeafSearchResult::Found(i) = self.search(key) {
            Some(&mut self.values[i])
        } else {
            None
        }
    }

    #[must_use]
    fn is_empty(&self) -> bool {
        match (self.keys.is_empty(), self.values.is_empty()) {
            (true, true) => true,
            (false, false) => false,
            _ => unreachable!(),
        }
    }

    #[must_use]
    fn is_full(&self) -> bool {
        match (self.keys.is_full(), self.values.is_full()) {
            (true, true) => true,
            (false, false) => false,
            _ => unreachable!(),
        }
    }

    fn assert_invariants(&self, is_root: bool) {
        debug_assert!(
            self.keys.len() == self.values.len(),
            "Invalid number of self.keys relative to self.values"
        );
        if !is_root {
            debug_assert!(
                self.keys.len() >= (self.keys.capacity() / 2) + (self.keys.capacity() % 2),
                "Keys is not half full"
            );
            debug_assert!(
                self.values.len() >= (self.values.capacity() / 2) + (self.values.capacity() % 2),
                "Values is not half full"
            );
        }
    }
}

enum NodeLeafSearchResult {
    Found(usize),
    CanInsert(usize),
    CannotInsert(usize),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct MyKey(usize);

    impl Key for MyKey {}

    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct MyValue(usize);

    impl Value for MyValue {}

    #[test]
    fn test() {
        let mut tree = BTree::<MyKey, MyValue>::new();
        assert!(tree.get(&MyKey(42)).is_none());
        assert!(tree.get_mut(&MyKey(42)).is_none());

        tree.insert(&MyKey(42), MyValue(4200));
        assert_eq!(tree.get(&MyKey(42)), Some(MyValue(4200)).as_ref());
        assert_eq!(tree.get_mut(&MyKey(42)), Some(MyValue(4200)).as_mut());

        let value = tree.remove(&MyKey(42));
        assert_eq!(value, Some(MyValue(4200)));
        assert!(tree.get(&MyKey(42)).is_none());
        assert!(tree.get_mut(&MyKey(42)).is_none());

        for i in 0..200 {
            tree.insert(&MyKey(i), MyValue(i * 100));
        }

        for i in 0..200 {
            assert_eq!(tree.get(&MyKey(i)), Some(MyValue(i * 100)).as_ref());
        }
    }
}
