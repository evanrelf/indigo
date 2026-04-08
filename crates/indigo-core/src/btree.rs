use arrayvec::ArrayVec;
use std::{mem, sync::Arc};

/// Ordered map based on an in-memory copy-on-write B+ tree
#[derive(Clone, Debug)]
pub struct BTreeMap<K, V> {
    root: Arc<Node<K, V>>,
    length: usize,
}

impl<K, V> BTreeMap<K, V> {
    /// Create a new, empty map
    pub fn new() -> Self {
        Self {
            root: Arc::new(Node::Leaf(NodeLeaf {
                keys: ArrayVec::new(),
                values: ArrayVec::new(),
            })),
            length: 0,
        }
    }

    /// Insert an entry, returning the existing value if present
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        match root.insert(key, value) {
            NodeInsertResult::Replaced(previous_value) => Some(previous_value),
            NodeInsertResult::Inserted => {
                self.length += 1;
                None
            }
            // Branch and leaf nodes implement insert-with-split by returning their sibling and the
            // key they'd like inserted in their parent. Only the root node is capable of creating
            // itself a new parent.
            NodeInsertResult::Split(parent_key, child_right) => {
                // Root node becomes the parent to the incoming `child_right` and its former self
                // (now called `child_left`).
                let child_left = {
                    let parent = Node::Branch(NodeBranch {
                        keys: ArrayVec::new(),
                        children: ArrayVec::new(),
                    });
                    mem::replace(root, parent)
                };

                let child_left_max = &child_left.keys()[child_left.keys().len() - 1];
                let child_right_min = &child_right.keys()[0];
                assert!(child_left_max < child_right_min, "Left child < right child");
                assert!(*child_left_max < parent_key, "Left child max < parent key");
                assert!(
                    child_right.is_branch() || *child_right_min == parent_key,
                    "Right child min == parent key"
                );

                let Node::Branch(parent) = root else {
                    unreachable!()
                };
                parent.keys.push(parent_key);
                parent.children.push(Arc::new(child_left));
                parent.children.push(Arc::new(child_right));

                self.length += 1;
                None
            }
        }
    }

    /// Remove an entry, returning the existing value if present
    ///
    /// We follow the advice of the ["Deletion without rebalancing in multiway search trees"][1]
    /// paper, which suggests "rebalancing on deletion not only is unnecessary but may be harmful."
    ///
    /// [1]: https://doi.org/10.1145/2540068
    pub fn remove(&mut self, key: &K) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        if let Some(value) = root.remove(key) {
            self.length -= 1;
            Some(value)
        } else {
            None
        }
    }

    /// Get a shared reference to the value associated with the given key
    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: Ord,
    {
        self.root.get(key)
    }

    /// Get an exclusive reference to the value associated with the given key
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        root.get_mut(key)
    }

    /// Check whether the key is present in the map
    pub fn contains_key(&self, key: &K) -> bool
    where
        K: Ord,
    {
        self.root.contains_key(key)
    }

    /// Return the number of entries in the map
    pub fn len(&self) -> usize {
        self.length
    }

    /// Check whether the map has any entries
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[expect(dead_code)]
    fn assert_invariants(&self)
    where
        K: Ord,
    {
        self.root.assert_invariants("root", 0);
    }
}

impl<K, V> Default for BTreeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> Extend<(K, V)> for BTreeMap<K, V>
where
    K: Clone + Ord,
    V: Clone,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = (K, V)>,
    {
        // TODO: Use more efficient bulk-loading algorithm:
        // https://cs186berkeley.net/notes/note4/#bulk-loading
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

#[cfg(not(test))]
const D: usize = 42; // TODO: Choose a real value
#[cfg(test)]
const D: usize = 2;

/// Maximum possible height of the tree
///
/// I'm making an educated guess that the height will will never exceed 38 based on these sources:
///
/// - [Wikipedia > B-tree > Best case and worst case heights][1]
/// - [WolframAlpha > "limit of (floor(log base 2 of ((n + 1) / 2))) as n approaches 1 trillion"][2]
///
/// [1]: https://en.wikipedia.org/wiki/B-tree#Best_case_and_worst_case_heights
/// [2]: https://www.wolframalpha.com/input?i=limit+of+%28floor%28log+base+2+of+%28%28n+%2B+1%29+%2F+2%29%29%29+as+n+approaches+1+trillion
const H_MAX: usize = 38;
const _: () = assert!(D >= 2, "`H_MAX` assumes `D` >= 2");

#[cfg_attr(not(test), expect(clippy::large_enum_variant))]
#[derive(Clone, Debug)]
enum Node<K, V> {
    Branch(NodeBranch<K, V>),
    Leaf(NodeLeaf<K, V>),
}

impl<K, V> Node<K, V> {
    fn insert(&mut self, key: K, value: V) -> NodeInsertResult<K, V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Node::Branch(branch) => branch.insert(key, value),
            Node::Leaf(leaf) => leaf.insert(key, value),
        }
    }

    fn remove(&mut self, key: &K) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Node::Branch(branch) => branch.remove(key),
            Node::Leaf(leaf) => leaf.remove(key),
        }
    }

    fn get(&self, key: &K) -> Option<&V>
    where
        K: Ord,
    {
        match self {
            Node::Branch(branch) => branch.get(key),
            Node::Leaf(leaf) => leaf.get(key),
        }
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Node::Branch(branch) => branch.get_mut(key),
            Node::Leaf(leaf) => leaf.get_mut(key),
        }
    }

    fn contains_key(&self, key: &K) -> bool
    where
        K: Ord,
    {
        match self {
            Node::Branch(branch) => branch.contains_key(key),
            Node::Leaf(leaf) => leaf.contains_key(key),
        }
    }

    fn keys(&self) -> &[K] {
        match self {
            Node::Branch(branch) => branch.keys.as_slice(),
            Node::Leaf(leaf) => leaf.keys.as_slice(),
        }
    }

    fn is_branch(&self) -> bool {
        matches!(self, Node::Branch(_))
    }

    #[cfg_attr(not(test), expect(dead_code))]
    fn is_leaf(&self) -> bool {
        matches!(self, Node::Leaf(_))
    }

    fn assert_invariants(&self, path: &str, depth: u8)
    where
        K: Ord,
    {
        match self {
            Node::Branch(branch) => branch.assert_invariants(path, depth),
            Node::Leaf(leaf) => leaf.assert_invariants(path, depth),
        }
    }
}

#[expect(dead_code)]
struct NodeSearchResult {
    branches: ArrayVec<usize, H_MAX>,
    leaf: LeafSearchResult,
}

#[cfg_attr(not(test), expect(clippy::large_enum_variant))]
enum NodeInsertResult<K, V> {
    Replaced(V),
    Inserted,
    Split(K, Node<K, V>),
}

#[derive(Clone, Debug)]
struct NodeBranch<K, V> {
    keys: ArrayVec<K, { (2 * D) + 1 }>,
    children: ArrayVec<Arc<Node<K, V>>, { (2 * D) + 2 }>,
}

impl<K, V> NodeBranch<K, V> {
    fn search(&self, key: &K) -> usize
    where
        K: Ord,
    {
        // TODO: Go back to binary search; it's better once the nodes get larger (e.g. >=4 KiB).
        self.keys
            .iter()
            .position(|k| k > key)
            .unwrap_or(self.children.len() - 1)
    }

    fn split(&mut self) -> (K, Self)
    where
        K: Clone,
    {
        assert!(self.is_overflowing(), "Only overflowing branches are split");
        let mut sibling = Self {
            keys: self.keys.drain(D..).collect(),
            children: self.children.drain(D + 1..).collect(),
        };
        let parent_key = sibling.keys.remove(0);
        assert_eq!(self.keys.len(), D);
        assert_eq!(self.children.len(), D + 1);
        assert_eq!(sibling.keys.len(), D);
        assert_eq!(sibling.children.len(), D + 1);
        (parent_key, sibling)
    }

    fn insert(&mut self, key: K, value: V) -> NodeInsertResult<K, V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let index = self.search(&key);
        let child = Arc::make_mut(&mut self.children[index]);
        match child.insert(key, value) {
            NodeInsertResult::Split(key, child) => {
                self.keys.insert(index, key);
                self.children.insert(index + 1, Arc::new(child));
                if self.is_overflowing() {
                    let (parent_key, sibling) = self.split();
                    NodeInsertResult::Split(parent_key, Node::Branch(sibling))
                } else {
                    NodeInsertResult::Inserted
                }
            }
            no_split => no_split,
        }
    }

    fn remove(&mut self, key: &K) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let index = self.search(key);
        let child = Arc::make_mut(&mut self.children[index]);
        child.remove(key)
    }

    fn get(&self, key: &K) -> Option<&V>
    where
        K: Ord,
    {
        let index = self.search(key);
        let child = &self.children[index];
        child.get(key)
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let index = self.search(key);
        let child = Arc::make_mut(&mut self.children[index]);
        child.get_mut(key)
    }

    fn contains_key(&self, key: &K) -> bool
    where
        K: Ord,
    {
        let index = self.search(key);
        let child = &self.children[index];
        child.contains_key(key)
    }

    #[expect(dead_code)]
    fn is_full(&self) -> bool {
        self.keys.len() + 1 == self.keys.capacity()
    }

    fn is_overflowing(&self) -> bool {
        self.keys.is_full()
    }

    fn assert_invariants(&self, path: &str, depth: u8)
    where
        K: Ord,
    {
        assert_eq!(
            self.keys.len() + 1,
            self.children.len(),
            "{path}: keys.len() + 1 != children.len()"
        );
        assert!(!self.is_overflowing(), "{path}: overflowing");
        // TODO: Assert all leaves are at the same depth.
        // TODO: Assert ordering is correct (e.g. keys to the left are less than). Could pass child
        // bounds and have it check itself.
        for (i, child) in self.children.iter().enumerate() {
            child.assert_invariants(
                &format!(
                    "{path}->{}{i}",
                    if child.is_branch() { "branch" } else { "leaf" }
                ),
                depth + 1,
            );
        }
    }
}

#[derive(Clone, Debug)]
struct NodeLeaf<K, V> {
    keys: ArrayVec<K, { (2 * D) + 1 }>,
    values: ArrayVec<V, { (2 * D) + 1 }>,
}

impl<K, V> NodeLeaf<K, V> {
    fn search(&self, key: &K) -> LeafSearchResult
    where
        K: Ord,
    {
        match self.keys.binary_search(key) {
            Ok(index) => LeafSearchResult::Found(index),
            Err(index) => LeafSearchResult::Missing(index),
        }
    }

    fn split(&mut self) -> (K, Self)
    where
        K: Clone,
    {
        assert!(self.is_overflowing(), "Only overflowing leaves are split");
        let self_len = D;
        let sibling_len = D + 1;
        let sibling = Self {
            keys: self.keys.drain(self_len..).collect(),
            values: self.values.drain(self_len..).collect(),
        };
        let parent_key = sibling.keys[0].clone();
        assert_eq!(self.keys.len(), self_len);
        assert_eq!(sibling.keys.len(), sibling_len);
        (parent_key, sibling)
    }

    fn insert(&mut self, key: K, value: V) -> NodeInsertResult<K, V>
    where
        K: Clone + Ord,
    {
        match self.search(&key) {
            LeafSearchResult::Found(index) => {
                let previous_value = mem::replace(&mut self.values[index], value);
                NodeInsertResult::Replaced(previous_value)
            }
            LeafSearchResult::Missing(index) => {
                self.keys.insert(index, key);
                self.values.insert(index, value);
                if self.is_overflowing() {
                    let (parent_key, sibling) = self.split();
                    NodeInsertResult::Split(parent_key, Node::Leaf(sibling))
                } else {
                    NodeInsertResult::Inserted
                }
            }
        }
    }

    fn remove(&mut self, key: &K) -> Option<V>
    where
        K: Ord,
    {
        match self.search(key) {
            LeafSearchResult::Found(index) => {
                self.keys.remove(index);
                let value = self.values.remove(index);
                Some(value)
            }
            _ => None,
        }
    }

    fn get(&self, key: &K) -> Option<&V>
    where
        K: Ord,
    {
        match self.search(key) {
            LeafSearchResult::Found(index) => Some(&self.values[index]),
            _ => None,
        }
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: Ord,
    {
        match self.search(key) {
            LeafSearchResult::Found(index) => Some(&mut self.values[index]),
            _ => None,
        }
    }

    fn contains_key(&self, key: &K) -> bool
    where
        K: Ord,
    {
        matches!(self.search(key), LeafSearchResult::Found(_))
    }

    #[expect(dead_code)]
    fn is_full(&self) -> bool {
        self.keys.len() + 1 == self.keys.capacity()
    }

    fn is_overflowing(&self) -> bool {
        self.keys.is_full()
    }

    fn assert_invariants(&self, path: &str, _depth: u8)
    where
        K: Ord,
    {
        assert!(
            self.keys.len() == self.values.len(),
            "{path}: keys.len() != values.len()"
        );
        assert!(!self.is_overflowing(), "{path}: overflowing");
        assert!(self.keys.is_sorted(), "{path}: keys are not sorted");
    }
}

enum LeafSearchResult {
    Found(usize),
    Missing(usize),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn xorshift(mut x: u32) -> u32 {
        assert!(x != 0);
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        x
    }

    #[test]
    fn big_insert() {
        let mut map = BTreeMap::new();
        let count: u32 = 1000;
        for x in (1..=count).map(xorshift) {
            map.insert(x, x);
        }
        assert_eq!(map.len(), 1000);
        for x in (1..=count).map(xorshift) {
            assert_eq!(map.remove(&x), Some(x));
        }
        assert!(map.is_empty());
    }

    #[test]
    fn leaf_split() {
        let mut leaf1 = NodeLeaf {
            keys: ArrayVec::new(),
            values: ArrayVec::new(),
        };

        assert!(matches!(leaf1.insert(1, 11), NodeInsertResult::Inserted));
        assert!(matches!(leaf1.insert(2, 22), NodeInsertResult::Inserted));
        assert!(matches!(leaf1.insert(3, 33), NodeInsertResult::Inserted));
        assert!(matches!(leaf1.insert(4, 44), NodeInsertResult::Inserted));
        let NodeInsertResult::Split(parent_key, Node::Leaf(leaf2)) = leaf1.insert(5, 55) else {
            panic!();
        };

        assert_eq!(leaf1.keys.as_slice(), &[1, 2]);
        assert_eq!(leaf1.values.as_slice(), &[11, 22]);

        assert_eq!(parent_key, 3);

        assert_eq!(leaf2.keys.as_slice(), &[3, 4, 5]);
        assert_eq!(leaf2.values.as_slice(), &[33, 44, 55]);
    }

    #[test]
    fn leaf_to_branch() {
        let mut map = BTreeMap::new();
        assert!(map.insert(1, 11).is_none());
        assert!(map.insert(2, 22).is_none());
        assert!(map.insert(3, 33).is_none());
        assert!(map.insert(4, 44).is_none());
        assert!(map.root.is_leaf());
        assert!(map.insert(5, 55).is_none());
        assert!(map.root.is_branch());
        assert_eq!(map.len(), 5);
    }
}
