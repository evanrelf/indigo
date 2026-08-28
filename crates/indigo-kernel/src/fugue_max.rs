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
                .inserted(0, initial_length)
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
        self.spans.iter().map(|span| span.visible_length()).sum()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.length() == 0
    }

    pub fn inserted(&mut self, byte_offset: usize, length: usize) -> anyhow::Result<Insertion> {
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

        let span_offset = self.split_at(byte_offset)?;

        let left_span = span_offset.checked_sub(1).map(|left| &self.spans[left]);
        let right_span = self.spans.get(span_offset);

        let insertion = Insertion {
            start: ByteId {
                author: self.id,
                sequence_number,
            },
            length,
            origin_left: left_span.map(|s| s.last()),
            origin_right: right_span.map(|s| s.start),
        };

        self.version_vector
            .record(insertion.start, insertion.length)?;

        let left_span = span_offset.checked_sub(1).map(|left| &mut self.spans[left]); // &mut

        match left_span {
            // If the left span exists and can absorb this insertion, then extend its length
            Some(left_span) if left_span.can_absorb(&insertion) => {
                left_span.length = left_span.length.checked_add(length.get()).unwrap();
            }
            // Otherwise, put the insertion in a new span
            _ => self.spans.insert(span_offset, Span::from(&insertion)),
        }

        Ok(insertion)
    }

    pub fn deleted(&mut self, byte_range: Range<usize>) -> anyhow::Result<Deletion> {
        if byte_range.start > byte_range.end || byte_range.end > self.length() {
            anyhow::bail!("invalid byte range {byte_range:?}");
        }

        let mut deletion_ranges = Vec::new();

        if byte_range.is_empty() {
            return Ok(Deletion(deletion_ranges));
        }

        let start_span_offset = self.split_at(byte_range.start)?;
        let end_span_offset = self.split_at(byte_range.end)?;

        for span_index in start_span_offset..end_span_offset {
            let span = &mut self.spans[span_index];
            if span.deleted {
                continue;
            }
            span.deleted = true;
            match deletion_ranges.last_mut() {
                Some(last) if last.end() == span.start => {
                    last.length = last.length.checked_add(span.length.get()).unwrap();
                }
                _ => deletion_ranges.push(DeletionRange {
                    start: span.start,
                    length: span.length,
                }),
            }
        }

        Ok(Deletion(deletion_ranges))
    }

    #[must_use]
    pub fn integrate_insertion(&mut self, insertion: &Insertion) -> Vec<Edit> {
        todo!()
    }

    #[must_use]
    pub fn integrate_deletion(&mut self, deletion: &Deletion) -> Vec<Edit> {
        todo!()
    }

    /// Convert a visible byte offset into a span offset, spitting any span the offset lands inside.
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
            } else if bytes_remaining >= wide(span.length) {
                // Offset at or past this span's end
                bytes_remaining -= wide(span.length);
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

    fn split_before(&mut self, byte: ByteId) -> anyhow::Result<usize> {
        let Some(span_index) = self.spans.iter().position(|span| span.contains(byte)) else {
            anyhow::bail!("no span contains byte");
        };
        let span = &mut self.spans[span_index];
        let byte_offset = byte.sequence_number - span.start.sequence_number;
        let Some(byte_offset) = NonZeroU32::new(byte_offset) else {
            assert_eq!(byte, self.spans[span_index].start);
            return Ok(span_index);
        };
        let new_span = span.split_off(byte_offset).expect("byte offset in bounds");
        self.spans.insert(span_index + 1, new_span);
        assert_eq!(byte, self.spans[span_index + 1].start);
        Ok(span_index + 1)
    }

    fn split_after(&mut self, byte: ByteId) -> anyhow::Result<usize> {
        let Some(span_index) = self.spans.iter().position(|span| span.contains(byte)) else {
            anyhow::bail!("no span contains byte");
        };
        let span = &mut self.spans[span_index];
        let byte_offset = (byte.sequence_number - span.start.sequence_number) + 1;
        let byte_offset = NonZeroU32::new(byte_offset).expect("if this wraps that's crazy");
        if let Some(new_span) = span.split_off(byte_offset) {
            // Byte was not already last of span, so split was necessary
            self.spans.insert(span_index + 1, new_span);
        }
        assert_eq!(byte, self.spans[span_index].last());
        Ok(span_index + 1)
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

    fn record(&mut self, start: ByteId, length: NonZeroU32) -> anyhow::Result<()> {
        if start.sequence_number != self.count(start.author) {
            anyhow::bail!("bytes must be recorded in order");
        }
        let Some(count) = length.checked_add(start.sequence_number) else {
            anyhow::bail!("inserting {length} bytes would overflow the version vector count");
        };
        if let Some((_, existing)) = self
            .0
            .iter_mut()
            .find(|(replica, _)| *replica == start.author)
        {
            *existing = count;
        } else {
            self.0.push((start.author, count));
        }
        Ok(())
    }
}

/// <https://en.wikipedia.org/wiki/Run-length_encoding>
#[derive(Clone, Debug)]
struct Span {
    start: ByteId,
    length: NonZeroU32,
    origin_left: Option<ByteId>,
    origin_right: Option<ByteId>,
    deleted: bool,
}

const _: () = assert!(std::mem::size_of::<Span>() == 32);

impl Span {
    fn contains(&self, byte: ByteId) -> bool {
        let is_same_author = self.start.author == byte.author;
        let sequence_start = self.start.sequence_number;
        let sequence_end = self
            .length
            .checked_add(self.start.sequence_number)
            .unwrap()
            .get();
        let is_in_sequence = (sequence_start..sequence_end).contains(&byte.sequence_number);
        is_same_author && is_in_sequence
    }

    fn end(&self) -> ByteId {
        self.start.plus(self.length.get())
    }

    fn last(&self) -> ByteId {
        self.start.plus(self.length.get() - 1)
    }

    fn visible_length(&self) -> usize {
        if self.deleted { 0 } else { wide(self.length) }
    }

    fn split_off(&mut self, byte_offset: NonZeroU32) -> Option<Self> {
        if byte_offset >= self.length {
            return None;
        }
        let other = Self {
            start: self.start.plus(byte_offset.get()),
            length: NonZeroU32::new(self.length.get() - byte_offset.get())
                .expect("not zero because of check above"),
            origin_left: Some(self.start.plus(byte_offset.get() - 1)),
            origin_right: self.origin_right,
            deleted: self.deleted,
        };
        self.length = byte_offset;
        Some(other)
    }

    fn can_absorb(&self, insertion: &Insertion) -> bool {
        !self.deleted
            // Insert follows span in time (replica inserted this next)
            && self.end() == insertion.start
            // Insert follows span in space (cursor started at end of span)
            && insertion.origin_left == Some(self.last())
            // No remote operation integrated here in the meantime
            && insertion.origin_right == self.origin_right
    }
}

impl From<&Insertion> for Span {
    fn from(insertion: &Insertion) -> Self {
        Self {
            start: insertion.start,
            length: insertion.length,
            origin_left: insertion.origin_left,
            origin_right: insertion.origin_right,
            deleted: false,
        }
    }
}

#[derive(Clone)]
pub struct Insertion {
    start: ByteId,
    length: NonZeroU32,
    origin_left: Option<ByteId>,
    origin_right: Option<ByteId>,
}

const _: () = assert!(std::mem::size_of::<Insertion>() == 28);

#[derive(Clone)]
pub struct Deletion(Vec<DeletionRange>);

const _: () = assert!(std::mem::size_of::<Deletion>() == 24);

#[derive(Clone)]
struct DeletionRange {
    start: ByteId,
    length: NonZeroU32,
}

impl DeletionRange {
    fn end(&self) -> ByteId {
        self.start.plus(self.length.get())
    }
}

const _: () = assert!(std::mem::size_of::<DeletionRange>() == 12);

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
    #![warn(unused)] // TODO: Remove

    use super::*;

    #[test]
    fn test_local_insertions() {
        let mut text = String::new();
        let mut replica = Replica::new(NonZeroU32::new(42).unwrap(), 0);

        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 0); // Initial length of 0 -> starts with no spans

        text.insert_str(0, "Hello");
        replica.inserted(0, "Hello".len()).unwrap();

        assert_eq!(&text, "Hello");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 1);

        text.insert(5, '!');
        replica.inserted(5, "!".len()).unwrap();

        assert_eq!(&text, "Hello!");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 1); // Appended to existing span

        text.insert_str(5, ", world");
        replica.inserted(5, ", world".len()).unwrap();

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
        replica.deleted(4..10).unwrap();

        assert_eq!(&text, "The brown fox");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 3); // [The ]{quick }[brown fox]
        assert_eq!(replica.spans.iter().filter(|s| s.deleted).count(), 1);

        // Delete across a tombstone
        text.replace_range(0..10, "");
        replica.deleted(0..10).unwrap();

        assert_eq!(&text, "fox");
        assert_eq!(text.len(), replica.length());
        assert_eq!(replica.spans.len(), 4); // {The }{quick }{brown }[fox]
        assert_eq!(replica.spans.iter().filter(|s| s.deleted).count(), 3);
    }
}
