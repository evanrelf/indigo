//! FugueMax CRDT algorithm.
//!
//! ## References:
//!
//! - The FugueMax paper: <https://arxiv.org/abs/2305.00583>
//! - Joseph Gentle's simple FugueMax implementation: <https://github.com/josephg/crdt-from-scratch>
//! - The `cola` CRDT, for its length- and operation-based API: <https://github.com/nomad/cola>

#![expect(clippy::doc_markdown)] // Clippy thinks "FugueMax" is missing backticks
#![allow(dead_code, unused_variables, unreachable_code)] // TODO: Remove

use std::{num::NonZeroU32, ops::Range};

pub struct Replica {
    id: ReplicaId,
    version_vector: VersionVector,
    spans: Vec<Span>,
    pending_insertions: Vec<Insertion>,
    pending_deletions: Vec<Deletion>,
}

const _: () = assert!(std::mem::size_of::<Replica>() == 104);

impl Replica {
    #[must_use]
    pub fn new(id: NonZeroU32, initial_length: usize) -> Self {
        let mut replica = Self {
            id: ReplicaId(id),
            spans: Vec::new(),
            version_vector: VersionVector::default(),
            pending_insertions: Vec::new(),
            pending_deletions: Vec::new(),
        };
        if initial_length > 0 {
            replica
                .local_insert(0, initial_length)
                .expect("document size <= 4 GiB");
        }
        replica
    }

    #[must_use]
    pub fn fork(&self, new_id: NonZeroU32) -> Self {
        Self {
            id: ReplicaId(new_id),
            spans: self.spans.clone(),
            version_vector: self.version_vector.clone(),
            pending_insertions: self.pending_insertions.clone(),
            pending_deletions: self.pending_deletions.clone(),
        }
    }

    #[must_use]
    pub fn length(&self) -> usize {
        self.spans
            .iter()
            .filter(|span| !span.deleted)
            .map(|span| wide(span.range.length))
            .sum()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.length() == 0
    }

    pub fn local_insert(&mut self, byte_offset: usize, length: usize) -> anyhow::Result<Insertion> {
        // Validate input
        let Ok(length) = u32::try_from(length) else {
            anyhow::bail!("insertion length {length} exceeds the maximum sequence number");
        };
        let Some(length) = NonZeroU32::new(length) else {
            anyhow::bail!("zero length, nothing to insert");
        };
        let sequence_number = self.version_vector.count(self.id);
        if length.checked_add(sequence_number).is_none() {
            anyhow::bail!("inserting {length} bytes would overflow the sequence number");
        }

        // Split span if necessary to accomodate insertion
        let span_offset = self.split_at(byte_offset)?;

        // Construct insertion
        let left_span = span_offset.checked_sub(1).map(|left| &self.spans[left]);
        let right_span = self.spans.get(span_offset);
        let insertion = Insertion {
            range: ByteRange {
                start: ByteId {
                    author: self.id,
                    sequence_number,
                },
                length,
            },
            origin_left: left_span.map(|s| s.range.last()),
            origin_right: right_span.map(|s| s.range.start),
        };

        // Apply insertion
        self.apply_insertion(span_offset, &insertion);

        Ok(insertion)
    }

    pub fn local_delete(&mut self, byte_range: Range<usize>) -> anyhow::Result<Deletion> {
        // Validate input
        if byte_range.start > byte_range.end || byte_range.end > self.length() {
            anyhow::bail!("invalid byte range {byte_range:?}");
        }
        if byte_range.is_empty() {
            anyhow::bail!("zero length, nothing to delete");
        }

        let mut deleted_ranges: Vec<ByteRange> = Vec::new();

        // Split spans if necessary to accomodate deletion
        let start_span_offset = self.split_at(byte_range.start)?;
        let end_span_offset = self.split_at(byte_range.end)?;

        // Loop over all the spans in the deletion byte range, ensuring they are deleted
        for span_index in start_span_offset..end_span_offset {
            let span = &mut self.spans[span_index];

            // If a span was already deleted, there's no further work to do
            if span.deleted {
                continue;
            }

            // Spans that haven't been deleted yet need to be marked as such, and returned as
            // deleted ranges to be propagated to peers and applied to the text
            span.deleted = true;
            match deleted_ranges.last_mut() {
                // If the previous deleted byte range exists and can absorb this deletion, then
                // extend its length
                Some(last) if last.end() == span.range.start => {
                    last.length = last.length.checked_add(span.range.length.get()).unwrap();
                }
                // Otherwise, start a new deleted byte range
                _ => deleted_ranges.push(span.range),
            }
        }

        Ok(Deletion(deleted_ranges))
    }

    #[must_use]
    pub fn remote_insert(&mut self, insertion: &Insertion) -> Vec<Edit> {
        if self.has_applied_insertion(insertion) {
            return Vec::new();
        }
        if !self.can_apply_insertion(insertion) {
            self.pending_insertions.push(insertion.clone());
            return Vec::new();
        }
        // TODO
        // let mut edits = vec![self.integrate_insertion(insertion)];
        let mut edits = vec![todo!()];
        edits.extend(self.drain_pending());
        edits
    }

    #[must_use]
    pub fn remote_delete(&mut self, deletion: &Deletion) -> Vec<Edit> {
        if !self.can_apply_deletion(deletion) {
            self.pending_deletions.push(deletion.clone());
            return Vec::new();
        }
        // TODO
        // self.integrate_deletion(deletion).into_iter().collect()
        todo!()
    }

    fn span_containing(&self, byte: ByteId) -> usize {
        self.spans
            .iter()
            .position(|span| span.range.contains(byte))
            .expect("byte is in version vector")
    }

    /// Convert a visible byte offset into a span offset, splitting any span the offset lands
    /// inside.
    ///
    /// ```text
    /// spans before: ["lorem ipsum", "dolor sit amet"]
    ///                     /\
    ///       split at visible byte offset 5
    ///
    /// spans after: ["lorem", " ipsum", "dolor sit amet"]
    ///                      /\
    ///                   return span offset 1
    /// ```
    fn split_at(&mut self, byte_offset: usize) -> anyhow::Result<usize> {
        let mut bytes_remaining = byte_offset;
        let mut span_offset = 0;
        while bytes_remaining > 0 {
            let Some(span) = self.spans.get(span_offset) else {
                anyhow::bail!("byte offset {byte_offset} is past the end of the document");
            };
            if span.deleted {
                // Skip over tombstoned spans
                span_offset += 1;
            } else if bytes_remaining >= wide(span.range.length) {
                // Offset at or past this span's end
                bytes_remaining -= wide(span.range.length);
                span_offset += 1;
            } else {
                // Offset in this span
                let at = u32::try_from(bytes_remaining).expect("smaller than a span length");
                let at = NonZeroU32::new(at).expect("nonzero while bytes_remaining > 0");
                let right = self.spans[span_offset]
                    .split_off(at)
                    .expect("at is mid-span");
                self.spans.insert(span_offset + 1, right);
                span_offset += 1;
                break;
            }
        }
        Ok(span_offset)
    }

    /// ```text
    /// spans before: ["lorem ipsum", "dolor sit amet"]
    ///                      ^
    ///         split before this ' ' byte
    ///
    /// spans after: ["lorem", " ipsum", "dolor sit amet"]
    ///                        ^^^^^^^^
    ///                   return span index 1
    /// ```
    fn split_before(&mut self, byte: ByteId) -> usize {
        let span_index = self.span_containing(byte);
        let span = &mut self.spans[span_index];
        let byte_offset = span.range.offset_of(byte).expect("span contains byte");
        let Some(byte_offset) = NonZeroU32::new(byte_offset) else {
            assert_eq!(byte, self.spans[span_index].range.start);
            return span_index;
        };
        let new_span = span.split_off(byte_offset).expect("byte offset in bounds");
        self.spans.insert(span_index + 1, new_span);
        assert_eq!(byte, self.spans[span_index + 1].range.start);
        span_index + 1
    }

    /// ```text
    /// spans before: ["lorem ipsum", "dolor sit amet"]
    ///                     ^
    ///         split after this 'm' byte
    ///
    /// spans after: ["lorem", " ipsum", "dolor sit amet"]
    ///                      /\
    ///                   return span offset 1
    /// ```
    fn split_after(&mut self, byte: ByteId) -> usize {
        let span_index = self.span_containing(byte);
        let span = &mut self.spans[span_index];
        let byte_offset = span.range.offset_of(byte).expect("span contains byte") + 1;
        let byte_offset = NonZeroU32::new(byte_offset).expect("if this wraps that's crazy");
        if let Some(new_span) = span.split_off(byte_offset) {
            // Byte was not already last of span, so split was necessary
            self.spans.insert(span_index + 1, new_span);
        }
        assert_eq!(byte, self.spans[span_index].range.last());
        // Returning a span offset, because this could've been the last byte in the document, so
        // `span_index + 1` might not exist.
        span_index + 1
    }

    fn has_applied_insertion(&self, insertion: &Insertion) -> bool {
        self.version_vector.contains(insertion.range.start)
    }

    // Whether all of an insertion's causal dependencies are in the document.
    fn can_apply_insertion(&self, insertion: &Insertion) -> bool {
        let start = insertion.range.start;
        let has_authors_prev = self.version_vector.count(start.author) == start.sequence_number;
        let has_origin_left = insertion
            .origin_left
            .is_none_or(|byte| self.version_vector.contains(byte));
        let has_origin_right = insertion
            .origin_right
            .is_none_or(|byte| self.version_vector.contains(byte));
        has_authors_prev && has_origin_left && has_origin_right
    }

    // Whether every byte a deletion targets is in the document.
    fn can_apply_deletion(&self, deletion: &Deletion) -> bool {
        deletion
            .0
            .iter()
            .all(|range| self.version_vector.contains(range.last()))
    }

    fn apply_insertion(&mut self, span_offset: usize, insertion: &Insertion) {
        self.version_vector
            .record(insertion.range.start, insertion.range.length);

        match span_offset.checked_sub(1).map(|left| &mut self.spans[left]) {
            // If the left span exists and can absorb this insertion, then extend its length
            Some(left_span) if left_span.can_absorb(insertion) => {
                left_span.range.length = left_span
                    .range
                    .length
                    .checked_add(insertion.range.length.get())
                    .expect("fits because the version vector accepted the same total");
            }
            // Otherwise, put the insertion in a new span
            _ => self.spans.insert(span_offset, Span::from(insertion)),
        }
    }

    fn drain_pending(&mut self) -> Vec<Edit> {
        let mut edits = Vec::new();
        loop {
            self.pending_insertions
                .retain(|insertion| !self.version_vector.contains(insertion.range.start));
            if let Some(index) = self
                .pending_insertions
                .iter()
                .position(|insertion| self.can_apply_insertion(insertion))
            {
                let insertion = self.pending_insertions.swap_remove(index);
                // TODO
                // edits.push(self.integrate_insertion(&insertion));
            } else if let Some(index) = self
                .pending_deletions
                .iter()
                .position(|deletion| self.can_apply_deletion(deletion))
            {
                let deletion = self.pending_deletions.swap_remove(index);
                // TODO
                // edits.extend(self.integrate_deletion(&deletion));
            } else {
                break;
            }
        }
        edits
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ReplicaId(NonZeroU32);

const _: () = assert!(std::mem::size_of::<ReplicaId>() == 4);
const _: () = assert!(std::mem::size_of::<Option<ReplicaId>>() == 4); // Niche used by `ByteId`

/// <https://en.wikipedia.org/wiki/Version_vector>
#[derive(Clone, Default)]
struct VersionVector(Vec<(ReplicaId, NonZeroU32)>);

const _: () = assert!(std::mem::size_of::<VersionVector>() == 24);
const _: () = assert!(std::mem::size_of::<(ReplicaId, NonZeroU32)>() == 8);

impl VersionVector {
    fn count(&self, replica: ReplicaId) -> u32 {
        self.0
            .iter()
            .find(|(r, _)| *r == replica)
            .map_or(0, |(_, count)| count.get())
    }

    fn contains(&self, byte: ByteId) -> bool {
        byte.sequence_number < self.count(byte.author)
    }

    fn record(&mut self, start: ByteId, length: NonZeroU32) {
        assert_eq!(
            start.sequence_number,
            self.count(start.author),
            "bytes must be recorded in order"
        );
        let count = length
            .checked_add(start.sequence_number)
            .expect("callers check for overflow before recording");
        if let Some((_, existing)) = self
            .0
            .iter_mut()
            .find(|(replica, _)| *replica == start.author)
        {
            *existing = count;
        } else {
            self.0.push((start.author, count));
        }
    }
}

/// <https://en.wikipedia.org/wiki/Run-length_encoding>
#[derive(Clone, Debug)]
struct Span {
    range: ByteRange,
    origin_left: Option<ByteId>,
    origin_right: Option<ByteId>,
    deleted: bool,
}

const _: () = assert!(std::mem::size_of::<Span>() == 32);

impl Span {
    fn split_off(&mut self, byte_offset: NonZeroU32) -> Option<Self> {
        let range = self.range.split_off(byte_offset)?;
        Some(Self {
            range,
            origin_left: Some(self.range.last()),
            origin_right: self.origin_right,
            deleted: self.deleted,
        })
    }

    fn can_absorb(&self, insertion: &Insertion) -> bool {
        !self.deleted
            // Insert follows span in time (replica inserted this next)
            && self.range.end() == insertion.range.start
            // Insert follows span in space (cursor started at end of span)
            && insertion.origin_left == Some(self.range.last())
            // No remote operation integrated here in the meantime
            && insertion.origin_right == self.origin_right
    }
}

impl From<&Insertion> for Span {
    fn from(insertion: &Insertion) -> Self {
        Self {
            range: insertion.range,
            origin_left: insertion.origin_left,
            origin_right: insertion.origin_right,
            deleted: false,
        }
    }
}

#[derive(Clone)]
pub struct Insertion {
    range: ByteRange,
    origin_left: Option<ByteId>,
    origin_right: Option<ByteId>,
}

const _: () = assert!(std::mem::size_of::<Insertion>() == 28);

#[derive(Clone)]
pub struct Deletion(Vec<ByteRange>);

const _: () = assert!(std::mem::size_of::<Deletion>() == 24);

/// Stable identifier for a byte, in time not in space; "the n-th byte this replica produced" not
/// "the byte at this position in the text"
#[derive(Clone, Copy, Debug, PartialEq)]
struct ByteId {
    /// Which replica inserted the byte.
    author: ReplicaId,
    /// When the author inserted the byte.
    sequence_number: u32,
}

const _: () = assert!(std::mem::size_of::<ByteId>() == 8);
const _: () = assert!(std::mem::size_of::<Option<ByteId>>() == 8); // Niche from `ReplicaId`

impl ByteId {
    fn plus(self, count: u32) -> Self {
        Self {
            author: self.author,
            sequence_number: self.sequence_number + count,
        }
    }
}

/// A run of bytes with consecutive sequence numbers from one author.
#[derive(Clone, Copy, Debug)]
struct ByteRange {
    start: ByteId,
    length: NonZeroU32,
}

const _: () = assert!(std::mem::size_of::<ByteRange>() == 12);

impl ByteRange {
    fn offset_of(&self, byte: ByteId) -> Option<u32> {
        if self.start.author != byte.author {
            return None;
        }
        let offset = byte
            .sequence_number
            .checked_sub(self.start.sequence_number)?;
        (offset < self.length.get()).then_some(offset)
    }

    fn contains(&self, byte: ByteId) -> bool {
        self.offset_of(byte).is_some()
    }

    fn end(&self) -> ByteId {
        self.start.plus(self.length.get())
    }

    fn last(&self) -> ByteId {
        self.start.plus(self.length.get() - 1)
    }

    fn split_off(&mut self, byte_offset: NonZeroU32) -> Option<Self> {
        if byte_offset >= self.length {
            return None;
        }
        let other = Self {
            start: self.start.plus(byte_offset.get()),
            length: NonZeroU32::new(self.length.get() - byte_offset.get())
                .expect("not zero because of check above"),
        };
        self.length = byte_offset;
        Some(other)
    }
}

// TODO: These fields don't make sense to me (yet?)
pub enum Edit {
    Insert {
        byte_offset: usize,
        author: ReplicaId,
        bytes: Range<usize>,
    },
    Delete {
        byte_ranges: Vec<Range<usize>>,
    },
}

const _: () = assert!(std::mem::size_of::<Edit>() == 32);

fn wide(n: impl Into<u32>) -> usize {
    usize::try_from(n.into()).expect("usize is at least 32 bits")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_local_insertions() {
        let mut text = String::new();
        let mut replica = Replica::new(NonZeroU32::new(42).unwrap(), 0);

        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 0); // Initial length of 0 -> starts with no spans

        text.insert_str(0, "Hello");
        replica.local_insert(0, "Hello".len()).unwrap();

        assert_eq!(&text, "Hello");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 1);

        text.insert(5, '!');
        replica.local_insert(5, "!".len()).unwrap();

        assert_eq!(&text, "Hello!");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 1); // Appended to existing span

        text.insert_str(5, ", world");
        replica.local_insert(5, ", world".len()).unwrap();

        assert_eq!(&text, "Hello, world!");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 3); // Existing span split + new span inserted between
    }

    #[test]
    fn test_local_deletions() {
        let mut text = String::from("The quick brown fox");
        let mut replica = Replica::new(NonZeroU32::new(42).unwrap(), text.len());

        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 1); // [The quick brown fox]
        assert_eq!(replica.spans.iter().filter(|s| s.deleted).count(), 0);

        text.replace_range(4..10, "");
        replica.local_delete(4..10).unwrap();

        assert_eq!(&text, "The brown fox");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 3); // [The ]{quick }[brown fox]
        assert_eq!(replica.spans.iter().filter(|s| s.deleted).count(), 1);

        // Delete across a tombstone
        text.replace_range(0..10, "");
        replica.local_delete(0..10).unwrap();

        assert_eq!(&text, "fox");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 4); // {The }{quick }{brown }[fox]
        assert_eq!(replica.spans.iter().filter(|s| s.deleted).count(), 3);
    }

    #[test]
    fn test_remote_insertions() {
        // TODO: Write unit test like `test_local_insertions` above
    }

    #[test]
    fn test_remote_deletions() {
        // TODO: Write unit test like `test_local_deletions` above
    }
}
