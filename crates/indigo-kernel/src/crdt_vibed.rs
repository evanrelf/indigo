//! FugueMax CRDT algorithm.
//!
//! ## References:
//!
//! - FugueMax paper: <https://arxiv.org/abs/2305.00583>
//! - Joseph Gentle's simple FugueMax implementation: <https://github.com/josephg/crdt-from-scratch>

#![expect(clippy::doc_markdown)] // Clippy thinks "FugueMax" is missing backticks

use std::{collections::HashMap, ops::Range};

pub struct Document {
    id: DocumentId,
    content: Vec<Item>,
    version: Version,
}

// Public API
impl Document {
    /// The id must be unique among documents that merge with each other.
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self {
            id: DocumentId(id),
            content: Vec::new(),
            version: Version::default(),
        }
    }

    #[must_use]
    pub fn text(&self) -> String {
        self.content
            .iter()
            .filter(|item| !item.deleted)
            .map(|item| item.content)
            .collect()
    }

    pub fn insert(&mut self, byte_index: usize, text: &str) -> anyhow::Result<()> {
        // Even inserting nothing demands a valid byte index.
        let index = self.insertion_index(byte_index)?;
        for (index, char) in (index..).zip(text.chars()) {
            self.insert_char_at(index, char);
        }
        Ok(())
    }

    pub fn delete(&mut self, byte_range: Range<usize>) -> anyhow::Result<()> {
        let text = self.text();
        if byte_range.start > byte_range.end
            || byte_range.end > text.len()
            || !text.is_char_boundary(byte_range.start)
            || !text.is_char_boundary(byte_range.end)
        {
            anyhow::bail!("invalid byte range {byte_range:?}");
        }
        let mut byte_index = 0;
        for item in &mut self.content {
            if item.deleted {
                continue;
            }
            if byte_range.contains(&byte_index) {
                item.deleted = true;
            }
            byte_index += item.content.len_utf8();
        }
        Ok(())
    }

    pub fn merge(&mut self, other: &Self) {
        // Integrate items we haven't seen, dependencies before dependents.
        let mut pending = other
            .content
            .iter()
            .filter(|item| !self.version.contains(item.id))
            .cloned()
            .collect::<Vec<Item>>();
        while !pending.is_empty() {
            let (ready, waiting): (Vec<Item>, Vec<Item>) = pending
                .into_iter()
                .partition(|item| self.can_integrate(item));
            assert!(
                !ready.is_empty(),
                "documents are causally complete, so every pass integrates something",
            );
            for item in ready {
                self.integrate(item);
            }
            pending = waiting;
        }

        // Propagate deletions. Note [Deletion propagation]
        let mut content = self.content.iter_mut();
        for item in other.content.iter().filter(|item| item.deleted) {
            let ours = content
                .by_ref()
                .find(|ours| ours.id == item.id)
                .expect("item exists");
            ours.deleted = true;
        }
    }
}

// Private API
impl Document {
    /// Inserts a freshly authored character at the given index in `content`,
    /// its physical neighbors becoming its origins.
    fn insert_char_at(&mut self, index: usize, char: char) {
        let item = Item {
            id: ItemId {
                document_id: self.id,
                sequence_number: self.version.next_sequence_number(self.id),
            },
            content: char,
            origin_left: index.checked_sub(1).map(|left| self.content[left].id),
            origin_right: self.content.get(index).map(|right| right.id),
            deleted: false,
        };
        self.integrate_between(item, index.checked_sub(1), index);
    }

    /// Where in `content` an insertion at the given byte index in the visible
    /// text lands. When deleted items straddle the position, insert to their
    /// left.
    fn insertion_index(&self, byte_index: usize) -> anyhow::Result<usize> {
        let mut remaining = byte_index;
        for (index, item) in self.content.iter().enumerate() {
            if remaining == 0 {
                return Ok(index);
            }
            if item.deleted {
                continue;
            }
            let Some(rest) = remaining.checked_sub(item.content.len_utf8()) else {
                anyhow::bail!("byte index {byte_index} is not a char boundary");
            };
            remaining = rest;
        }
        if remaining == 0 {
            Ok(self.content.len())
        } else {
            anyhow::bail!("byte index {byte_index} is past the end of the document");
        }
    }

    fn index_of(&self, id: ItemId) -> usize {
        self.content
            .iter()
            .position(|item| item.id == id)
            .expect("item exists")
    }

    /// Whether all of an item's causal dependencies are in this document: the
    /// previous item from its document of origin, and both origins.
    fn can_integrate(&self, item: &Item) -> bool {
        let previous = item
            .id
            .sequence_number
            .checked_sub(1)
            .map(|sequence_number| ItemId {
                document_id: item.id.document_id,
                sequence_number,
            });
        !self.version.contains(item.id)
            && previous.is_none_or(|id| self.version.contains(id))
            && item.origin_left.is_none_or(|id| self.version.contains(id))
            && item.origin_right.is_none_or(|id| self.version.contains(id))
    }

    fn integrate(&mut self, item: Item) {
        let left = item.origin_left.map(|id| self.index_of(id));
        let right = item
            .origin_right
            .map_or(self.content.len(), |id| self.index_of(id));
        self.integrate_between(item, left, right);
    }

    // Note [Integration scan]
    fn integrate_between(&mut self, item: Item, left: Option<usize>, right: usize) {
        self.version.record(item.id);

        let mut destination = left.map_or(0, |index| index + 1);
        let mut scanning = false;
        for index in destination.. {
            if !scanning {
                destination = index;
            }
            if index == self.content.len() || index == right {
                break;
            }
            let other = &self.content[index];
            let other_left = other.origin_left.map(|id| self.index_of(id));
            let other_right = other
                .origin_right
                .map_or(self.content.len(), |id| self.index_of(id));
            if other_left < left
                || (other_left == left
                    && other_right == right
                    && item.id.document_id < other.id.document_id)
            {
                break;
            }
            if other_left == left {
                scanning = other_right < right;
            }
        }
        self.content.insert(destination, item);
    }
}

#[cfg(test)]
impl Document {
    /// Every item's author, sequence number, and tombstone flag, in document
    /// order — for cross-implementation equivalence tests.
    pub(crate) fn item_summaries(&self) -> Vec<(usize, usize, bool)> {
        self.content
            .iter()
            .map(|item| (item.id.document_id.0, item.id.sequence_number, item.deleted))
            .collect()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
struct DocumentId(usize);

#[derive(Clone, Copy, PartialEq)]
struct ItemId {
    document_id: DocumentId,
    sequence_number: usize,
}

#[derive(Clone)]
struct Item {
    id: ItemId,
    content: char,
    origin_left: Option<ItemId>,
    origin_right: Option<ItemId>,
    deleted: bool,
}

#[derive(Default)]
struct Version(HashMap<DocumentId, usize>);

impl Version {
    fn contains(&self, id: ItemId) -> bool {
        self.0
            .get(&id.document_id)
            .is_some_and(|highest| *highest >= id.sequence_number)
    }

    /// Items from a document must be recorded in order, without gaps.
    fn record(&mut self, id: ItemId) {
        assert_eq!(
            id.sequence_number,
            self.next_sequence_number(id.document_id),
            "items must be recorded in order",
        );
        self.0.insert(id.document_id, id.sequence_number);
    }

    fn next_sequence_number(&self, document_id: DocumentId) -> usize {
        self.0.get(&document_id).map_or(0, |highest| highest + 1)
    }
}

/*
Note [Integration scan]
-----------------------

Claude: `integrate_between` places a new item among its rivals: items inserted
concurrently into the same gap, between the same left and right origins. The
scan starts just after the item's left origin and walks forward, stopping at
the latest by its right origin (or the end of the document). `destination`
trails the walk, marking where the item would be spliced if the scan stopped
now.

Each item passed is judged by its origins, mapped to indexes so they compare
like document positions. A left origin of `None` is the virtual item before the
document, and `Option`'s derived ordering conveniently puts `None` first, just
like the reference implementation's `-1` sentinel. An item whose left origin is
earlier than ours closes the gap we were inserted into: we belong before it. An
item whose left origin is later sits inside an earlier rival's subtree: we skip
past it. Items sharing our left origin are true siblings, ordered by their
right origins (later first), with ties broken by document id, smaller first.

A sibling whose right origin is earlier than ours might come before or after us
depending on what the scan finds next, so the walk enters a "scanning" state
that leaves `destination` pinned behind: if the scan later decides the item
belongs before the ambiguous run, it splices at the pinned spot, and if it
instead finds a sibling proving the item belongs after, `destination` catches
back up.

Remote items enter through `integrate`, which finds their origins' indexes by
searching for the origins' ids. Local insertions skip that: a freshly authored
item's origins are its physical neighbors, so `insert_char_at` already holds
their indexes and calls `integrate_between` directly. With `right` immediately
adjacent to the left origin, the scan stops before examining anyone, and the
item lands exactly where the user put it.

Note [Deletion propagation]
---------------------------

Claude: `merge` propagates the other document's deletions in a single forward
pass, one cursor over each document, never backing up. That works because the
algorithm defines one total order over all items, determined only by their
origins and ids, and every document displays whichever items it holds in that
order. After integration this document holds a superset of the other's items,
so the other's items appear here in the same relative order, and each next
deleted item can only lie ahead of the last one found.
*/

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::generators as gs;

    #[test]
    fn ties_broken_by_smaller_document_id() -> anyhow::Result<()> {
        let mut doc1 = Document::new(1);
        let mut doc2 = Document::new(2);

        doc1.insert(0, "A")?;
        doc2.insert(0, "B")?;

        doc1.merge(&doc2);
        doc2.merge(&doc1);

        assert_eq!(doc1.text(), "AB");
        assert_eq!(doc2.text(), "AB");
        Ok(())
    }

    /// Draws a byte index in `[0, text.len()]` lying on a `char` boundary.
    fn draw_boundary(tc: &hegel::TestCase, text: &str) -> usize {
        let mut index = tc.draw(gs::integers::<usize>().max_value(text.len()));
        while !text.is_char_boundary(index) {
            index += 1;
        }
        index
    }

    /// Draws a random valid edit and applies it to both the document and a
    /// plain `String` model of its visible text.
    fn edit_both(tc: &hegel::TestCase, doc: &mut Document, model: &mut String) {
        if model.is_empty() || tc.draw(gs::booleans()) {
            let index = draw_boundary(tc, model);
            let text: String = tc.draw(gs::text());
            doc.insert(index, &text).unwrap();
            model.insert_str(index, &text);
        } else {
            let one = draw_boundary(tc, model);
            let two = draw_boundary(tc, model);
            let range = one.min(two)..one.max(two);
            doc.delete(range.clone()).unwrap();
            model.replace_range(range, "");
        }
    }

    /// Draws a few random edits and applies them to the document.
    fn edit_randomly(tc: &hegel::TestCase, doc: &mut Document) {
        let edits = tc.draw(gs::integers::<usize>().max_value(3));
        for _ in 0..edits {
            let mut model = doc.text();
            edit_both(tc, doc, &mut model);
        }
    }

    /// Every item's id, character, and tombstone flag, in order. Converged
    /// documents must agree on this, not just on visible text: a hidden
    /// disagreement about tombstone order only surfaces as divergence on some
    /// later edit.
    fn items(doc: &Document) -> Vec<(usize, usize, char, bool)> {
        doc.content
            .iter()
            .map(|item| {
                (
                    item.id.document_id.0,
                    item.id.sequence_number,
                    item.content,
                    item.deleted,
                )
            })
            .collect()
    }

    /// Types `text` into the document one character at a time, either forward
    /// ("abc" typed a, b, c after each other) or backward ("abc" built c, b, a
    /// with every insert at the same position).
    fn type_at(tc: &hegel::TestCase, doc: &mut Document, byte_index: usize, text: &str) {
        if tc.draw(gs::booleans()) {
            let mut byte_index = byte_index;
            for char in text.chars() {
                doc.insert(byte_index, &char.to_string()).unwrap();
                byte_index += char.len_utf8();
            }
        } else {
            for char in text.chars().rev() {
                doc.insert(byte_index, &char.to_string()).unwrap();
            }
        }
    }

    /// `docs[into].merge(&docs[from])`, which the borrow checker only allows
    /// through a split.
    fn merge_pair(docs: &mut [Document], into: usize, from: usize) {
        if into < from {
            let (left, right) = docs.split_at_mut(from);
            left[into].merge(&right[0]);
        } else {
            let (left, right) = docs.split_at_mut(into);
            right[0].merge(&left[from]);
        }
    }

    #[hegel::test(test_cases = 2_000)]
    fn local_edits_match_string(tc: hegel::TestCase) {
        let mut doc = Document::new(1);
        let mut model = String::new();
        let edits = tc.draw(gs::integers::<usize>().max_value(20));
        for _ in 0..edits {
            edit_both(&tc, &mut doc, &mut model);
            assert_eq!(doc.text(), model);
        }
    }

    #[hegel::test(test_cases = 2_000)]
    fn concurrent_edits_converge(tc: hegel::TestCase) {
        let count = tc.draw(gs::integers::<usize>().min_value(2).max_value(4));
        let mut docs = (1..=count).map(Document::new).collect::<Vec<Document>>();

        let rounds = tc.draw(gs::integers::<usize>().min_value(1).max_value(3));
        for _ in 0..rounds {
            // Everyone edits concurrently...
            for doc in &mut docs {
                edit_randomly(&tc, doc);
            }
            // ...then two documents gossip.
            let into = tc.draw(gs::integers::<usize>().max_value(count - 1));
            let from = tc.draw(gs::integers::<usize>().max_value(count - 1));
            if into != from {
                merge_pair(&mut docs, into, from);
            }
        }

        // After merging all-to-all, every document has seen every edit.
        for into in 0..count {
            for from in 0..count {
                if into != from {
                    merge_pair(&mut docs, into, from);
                }
            }
        }
        for pair in docs.windows(2) {
            assert_eq!(items(&pair[0]), items(&pair[1]));
        }
    }

    /// FugueMax's defining guarantee: runs of text typed concurrently at the
    /// same position never interleave. The disjoint alphabets make the
    /// substring checks exact.
    #[hegel::test(test_cases = 2_000)]
    fn concurrent_typing_does_not_interleave(tc: hegel::TestCase) {
        let base: String = tc.draw(gs::text().alphabet("xyz"));
        let mut doc1 = Document::new(1);
        doc1.insert(0, &base).unwrap();
        let mut doc2 = Document::new(2);
        doc2.merge(&doc1);

        let byte_index = draw_boundary(&tc, &base);
        let text1: String = tc.draw(gs::text().alphabet("ABC").min_size(1));
        let text2: String = tc.draw(gs::text().alphabet("123").min_size(1));
        type_at(&tc, &mut doc1, byte_index, &text1);
        type_at(&tc, &mut doc2, byte_index, &text2);

        doc1.merge(&doc2);
        doc2.merge(&doc1);
        assert_eq!(items(&doc1), items(&doc2));
        assert!(doc1.text().contains(&text1));
        assert!(doc1.text().contains(&text2));
    }

    #[hegel::test(test_cases = 2_000)]
    fn merge_is_idempotent(tc: hegel::TestCase) {
        let mut doc1 = Document::new(1);
        let mut doc2 = Document::new(2);
        edit_randomly(&tc, &mut doc1);
        edit_randomly(&tc, &mut doc2);

        // Share some history, then diverge again, so re-merging must skip
        // partially overlapping versions rather than a wholly foreign document.
        doc1.merge(&doc2);
        doc2.merge(&doc1);
        edit_randomly(&tc, &mut doc1);
        edit_randomly(&tc, &mut doc2);

        doc1.merge(&doc2);
        let merged = items(&doc1);
        doc1.merge(&doc2);
        assert_eq!(items(&doc1), merged);
    }

    #[hegel::test(test_cases = 2_000)]
    fn invalid_edits_error_and_change_nothing(tc: hegel::TestCase) {
        let mut doc = Document::new(1);
        let initial: String = tc.draw(gs::text());
        doc.insert(0, &initial).unwrap();
        let mut model = doc.text();

        // Insert at an arbitrary byte index, valid or not.
        let index = tc.draw(hegel::one_of!(
            gs::integers::<usize>().max_value(model.len() + 1),
            gs::integers::<usize>(),
        ));
        let text: String = tc.draw(gs::text());
        if model.is_char_boundary(index) {
            doc.insert(index, &text).unwrap();
            model.insert_str(index, &text);
        } else {
            assert!(doc.insert(index, &text).is_err());
        }
        assert_eq!(doc.text(), model);

        // Delete an arbitrary byte range, valid or not.
        let start = tc.draw(hegel::one_of!(
            gs::integers::<usize>().max_value(model.len() + 1),
            gs::integers::<usize>(),
        ));
        let end = tc.draw(hegel::one_of!(
            gs::integers::<usize>().max_value(model.len() + 1),
            gs::integers::<usize>(),
        ));
        if start <= end && model.is_char_boundary(start) && model.is_char_boundary(end) {
            doc.delete(start..end).unwrap();
            model.replace_range(start..end, "");
        } else {
            assert!(doc.delete(start..end).is_err());
        }
        assert_eq!(doc.text(), model);
    }
}
