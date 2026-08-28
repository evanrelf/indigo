//! Length-based FugueMax CRDT.
//!
//! Unlike [`crate::crdt_vibed`], the CRDT here stores no text at all — only
//! byte lengths and byte positions. Local edits return
//! [`Insertion`]/[`Deletion`] values to send to other replicas, and
//! integrating a remote op returns [`BufferEdit`]s telling the caller what to
//! do to its own text buffer. Note \[Spans\]
//!
//! ## References:
//!
//! - The FugueMax paper: <https://arxiv.org/abs/2305.00583>
//! - Joseph Gentle's simple FugueMax implementation: <https://github.com/josephg/crdt-from-scratch>
//! - cola, whose length-based, operation-based API this module borrows
//!   (but not its RGA-style ordering): <https://github.com/nomad/cola>

#![expect(clippy::doc_markdown)] // Clippy thinks "FugueMax" is missing backticks

use std::{num::NonZeroU32, ops::Range};

/// Widens a stored `u32` to a spatial `usize`; infallible on the platforms
/// indigo supports.
fn wide(bytes: u32) -> usize {
    usize::try_from(bytes).expect("usize is at least 32 bits")
}

#[derive(Clone)]
pub struct Replica {
    id: ReplicaId,
    spans: Vec<Span>,
    version: Version,
    // Note [Pending ops]
    pending_insertions: Vec<Insertion>,
    pending_deletions: Vec<Deletion>,
}

// Public API
impl Replica {
    /// The id must be unique among replicas of the same document. The initial
    /// content is credited to this replica's clock, so replicas joining an
    /// existing session must [`fork`](Self::fork) one that's already in it
    /// rather than call `new` themselves.
    #[must_use]
    pub fn new(id: NonZeroU32, initial_length: usize) -> Self {
        let mut replica = Self {
            id: ReplicaId(id),
            spans: Vec::new(),
            version: Version::default(),
            pending_insertions: Vec::new(),
            pending_deletions: Vec::new(),
        };
        replica
            .inserted(0, initial_length)
            .expect("byte offset 0 is valid and the initial length fits the clock");
        replica
    }

    /// A copy of this replica that authors its own edits under a new id.
    #[must_use]
    pub fn fork(&self, new_id: NonZeroU32) -> Self {
        Self {
            id: ReplicaId(new_id),
            ..self.clone()
        }
    }

    /// The visible length of the document, in bytes.
    #[must_use]
    pub fn len(&self) -> usize {
        self.spans.iter().map(Span::visible_length).sum()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Informs the replica that `length` bytes were inserted at the given
    /// visible offset in the caller's buffer, returning the op to send to the
    /// other replicas.
    pub fn inserted(&mut self, byte_offset: usize, length: usize) -> anyhow::Result<Insertion> {
        let Ok(length) = u32::try_from(length) else {
            anyhow::bail!("insertion length {length} exceeds the u32 byte clock");
        };
        let byte_index = self.version.count(self.id);
        if byte_index.checked_add(length).is_none() {
            anyhow::bail!("inserting {length} bytes would overflow the u32 byte clock");
        }

        // Find the gap in `spans` at the given offset, splitting the span it
        // lands inside. When deleted spans straddle the position, insert to
        // their left.
        let mut remaining = byte_offset;
        let mut index = 0;
        while remaining > 0 {
            let Some(span) = self.spans.get(index) else {
                anyhow::bail!("byte offset {byte_offset} is past the end of the document");
            };
            if !span.deleted {
                if remaining < wide(span.length) {
                    let at = u32::try_from(remaining).expect("fits in a span");
                    let right = self.spans[index].split(at);
                    self.spans.insert(index + 1, right);
                    index += 1;
                    break;
                }
                remaining -= wide(span.length);
            }
            index += 1;
        }

        let op = Insertion {
            start: ByteId {
                replica: self.id,
                byte_index,
            },
            length,
            origin_left: index.checked_sub(1).map(|left| self.spans[left].last_id()),
            origin_right: self.spans.get(index).map(|right| right.id),
        };
        self.version.record(op.start, op.length);
        // The origins are the physical neighbors, so no integration scan is
        // needed: the span lands exactly where the caller put it, extending
        // its predecessor when it simply continues it. Note [Integration scan]
        if length > 0 {
            match index.checked_sub(1) {
                Some(left) if self.spans[left].can_append(&op) => {
                    self.spans[left].length += length;
                }
                _ => self.spans.insert(index, Span::of(&op)),
            }
        }
        Ok(op)
    }

    /// Informs the replica that the given visible byte range was deleted from
    /// the caller's buffer, returning the op to send to the other replicas.
    /// Note [Deletion by id]
    pub fn deleted(&mut self, byte_range: Range<usize>) -> anyhow::Result<Deletion> {
        if byte_range.start > byte_range.end || byte_range.end > self.len() {
            anyhow::bail!("invalid byte range {byte_range:?}");
        }
        let mut targets = Vec::<Run>::new();
        if byte_range.is_empty() {
            return Ok(Deletion { targets });
        }
        let mut visible = 0;
        let mut index = 0;
        while index < self.spans.len() && visible < byte_range.end {
            let span = &self.spans[index];
            if span.deleted {
                index += 1;
                continue;
            }
            if visible + wide(span.length) <= byte_range.start {
                visible += wide(span.length);
                index += 1;
                continue;
            }
            // The range starts mid-span: split off the head and revisit.
            let head = byte_range.start.saturating_sub(visible);
            if head > 0 {
                let at = u32::try_from(head).expect("fits in a span");
                let right = self.spans[index].split(at);
                self.spans.insert(index + 1, right);
                visible += head;
                index += 1;
                continue;
            }
            // The span starts inside the range: split off any tail past it,
            // then the span is wholly covered.
            let take = (byte_range.end - visible).min(wide(self.spans[index].length));
            if take < wide(self.spans[index].length) {
                let at = u32::try_from(take).expect("fits in a span");
                let right = self.spans[index].split(at);
                self.spans.insert(index + 1, right);
            }
            let span = &mut self.spans[index];
            span.deleted = true;
            match targets.last_mut() {
                Some(run) if run.end() == span.id => run.length += span.length,
                _ => targets.push(Run {
                    start: span.id,
                    length: span.length,
                }),
            }
            visible += wide(span.length);
            index += 1;
        }
        Ok(Deletion { targets })
    }

    /// Integrates another replica's [`Insertion`], returning the edits to
    /// apply to the caller's buffer, in order: the op itself and any pending
    /// ops it unblocked. Empty when the op is a duplicate, empty, or must wait
    /// for its causal dependencies. Note [Pending ops]
    #[must_use]
    pub fn integrate_insertion(&mut self, op: &Insertion) -> Vec<BufferEdit> {
        if op.length == 0 || self.has_applied(op) {
            return Vec::new();
        }
        if !self.can_apply_insertion(op) {
            self.pending_insertions.push(op.clone());
            return Vec::new();
        }
        let mut edits = vec![self.apply_insertion(op)];
        self.drain_pending(&mut edits);
        edits
    }

    /// Integrates another replica's [`Deletion`], returning the edits to apply
    /// to the caller's buffer. Empty when nothing new was deleted or the op
    /// must wait for its causal dependencies. Note [Deletion by id]
    #[must_use]
    pub fn integrate_deletion(&mut self, op: &Deletion) -> Vec<BufferEdit> {
        if !self.can_apply_deletion(op) {
            self.pending_deletions.push(op.clone());
            return Vec::new();
        }
        match self.apply_deletion(op) {
            Some(edit) => vec![edit],
            None => Vec::new(),
        }
    }
}

// Private API
impl Replica {
    fn span_containing(&self, id: ByteId) -> usize {
        self.spans
            .iter()
            .position(|span| span.contains(id))
            .expect("byte exists")
    }

    /// The position `id` would have if every span were expanded to one item
    /// per byte. Note [Integration scan]
    fn position_of(&self, id: ByteId) -> usize {
        let mut position = 0;
        for span in &self.spans {
            if span.contains(id) {
                return position + wide(id.byte_index - span.id.byte_index);
            }
            position += wide(span.length);
        }
        unreachable!("byte exists")
    }

    /// The expanded position one past every byte in the document.
    fn expansion_len(&self) -> usize {
        self.spans.iter().map(|span| wide(span.length)).sum()
    }

    /// The number of visible bytes before the span at `index`.
    fn visible_prefix(&self, index: usize) -> usize {
        self.spans[..index].iter().map(Span::visible_length).sum()
    }

    /// Splits so `id` is the last byte of its span, returning that span's
    /// index.
    fn split_after(&mut self, id: ByteId) -> usize {
        let index = self.span_containing(id);
        let keep = id.byte_index - self.spans[index].id.byte_index + 1;
        if keep < self.spans[index].length {
            let right = self.spans[index].split(keep);
            self.spans.insert(index + 1, right);
        }
        index
    }

    /// Splits so `id` is the first byte of its span, returning that span's
    /// index.
    fn split_before(&mut self, id: ByteId) -> usize {
        let index = self.span_containing(id);
        let keep = id.byte_index - self.spans[index].id.byte_index;
        if keep == 0 {
            index
        } else {
            let right = self.spans[index].split(keep);
            self.spans.insert(index + 1, right);
            index + 1
        }
    }

    /// Whether this insertion is already part of the document. Ops are atomic:
    /// seeing the first byte means seeing them all.
    fn has_applied(&self, op: &Insertion) -> bool {
        self.version.contains(op.start)
    }

    /// Whether all of an insertion's causal dependencies are in this document:
    /// its author's previous bytes, and both origins.
    fn can_apply_insertion(&self, op: &Insertion) -> bool {
        self.version.count(op.start.replica) == op.start.byte_index
            && op.origin_left.is_none_or(|id| self.version.contains(id))
            && op.origin_right.is_none_or(|id| self.version.contains(id))
    }

    /// Whether every byte a deletion targets is in this document.
    fn can_apply_deletion(&self, op: &Deletion) -> bool {
        op.targets
            .iter()
            .all(|run| self.version.contains(run.last()))
    }

    // Note [Integration scan]
    fn apply_insertion(&mut self, op: &Insertion) -> BufferEdit {
        let left = op.origin_left.map(|id| self.split_after(id));
        let right = op
            .origin_right
            .map_or(self.spans.len(), |id| self.split_before(id));

        let left_position = op.origin_left.map(|id| self.position_of(id));
        let right_position = op
            .origin_right
            .map_or(self.expansion_len(), |id| self.position_of(id));
        let mut destination = left.map_or(0, |index| index + 1);
        let mut scanning = false;
        for index in destination.. {
            if !scanning {
                destination = index;
            }
            if index == self.spans.len() || index == right {
                break;
            }
            let other = &self.spans[index];
            let other_left = other.origin_left.map(|id| self.position_of(id));
            let other_right = other
                .origin_right
                .map_or(self.expansion_len(), |id| self.position_of(id));
            if other_left < left_position
                || (other_left == left_position
                    && other_right == right_position
                    && op.start.replica < other.id.replica)
            {
                break;
            }
            if other_left == left_position {
                scanning = other_right < right_position;
            }
        }

        self.version.record(op.start, op.length);
        let byte_offset = self.visible_prefix(destination);
        match destination.checked_sub(1) {
            Some(previous) if self.spans[previous].can_append(op) => {
                self.spans[previous].length += op.length;
            }
            _ => self.spans.insert(destination, Span::of(op)),
        }
        BufferEdit::Insert {
            byte_offset,
            author: op.start.replica,
            bytes: op.bytes(),
        }
    }

    /// Marks the targeted bytes deleted, reporting the visible ranges of the
    /// bytes this call actually hid — in pre-deletion coordinates, ascending —
    /// or `None` if every target was already dead. Note [Deletion by id]
    fn apply_deletion(&mut self, op: &Deletion) -> Option<BufferEdit> {
        let mut byte_ranges = Vec::<Range<usize>>::new();
        let mut visible = 0;
        let mut index = 0;
        while index < self.spans.len() {
            let span = &self.spans[index];
            if span.deleted {
                index += 1;
                continue;
            }
            let Some((from, to)) = op.earliest_overlap(span) else {
                visible += wide(span.length);
                index += 1;
                continue;
            };
            // The overlap starts mid-span: split off the head and revisit.
            if from > 0 {
                let right = self.spans[index].split(from);
                self.spans.insert(index + 1, right);
                visible += wide(from);
                index += 1;
                continue;
            }
            // The span starts inside the overlap: split off any tail past it,
            // then the span is wholly covered. Any further overlaps with the
            // tail are found when the walk reaches it.
            if to < self.spans[index].length {
                let right = self.spans[index].split(to);
                self.spans.insert(index + 1, right);
            }
            let span = &mut self.spans[index];
            span.deleted = true;
            match byte_ranges.last_mut() {
                Some(range) if range.end == visible => range.end += wide(span.length),
                _ => byte_ranges.push(visible..visible + wide(span.length)),
            }
            visible += wide(span.length);
            index += 1;
        }
        (!byte_ranges.is_empty()).then_some(BufferEdit::Delete { byte_ranges })
    }

    // Note [Pending ops]
    fn drain_pending(&mut self, edits: &mut Vec<BufferEdit>) {
        loop {
            // An op delivered repeatedly while unready is stashed repeatedly;
            // once one copy applies, the rest become duplicates. Drop them.
            let version = &self.version;
            self.pending_insertions
                .retain(|op| !version.contains(op.start));

            if let Some(index) = self
                .pending_insertions
                .iter()
                .position(|op| self.can_apply_insertion(op))
            {
                let op = self.pending_insertions.swap_remove(index);
                edits.push(self.apply_insertion(&op));
            } else if let Some(index) = self
                .pending_deletions
                .iter()
                .position(|op| self.can_apply_deletion(op))
            {
                let op = self.pending_deletions.swap_remove(index);
                edits.extend(self.apply_deletion(&op));
            } else {
                return;
            }
        }
    }
}

/// A replica's insertion of a run of bytes, in CRDT coordinates. Carries no
/// text: the caller pairs the op with the inserted text, identified by
/// [`author`](Self::author) and [`bytes`](Self::bytes).
#[derive(Clone)]
pub struct Insertion {
    /// The first byte's id; the rest of the run follows it temporally.
    start: ByteId,
    length: u32,
    /// The origin of the run's first byte.
    origin_left: Option<ByteId>,
    /// The origin shared by every byte in the run.
    origin_right: Option<ByteId>,
}

impl Insertion {
    /// The replica that authored this insertion.
    #[must_use]
    pub fn author(&self) -> ReplicaId {
        self.start.replica
    }

    /// The temporal range of the inserted bytes in the author's byte clock.
    /// Together with [`author`](Self::author) this uniquely identifies the
    /// insertion — and therefore its text — across all replicas.
    #[must_use]
    pub fn bytes(&self) -> Range<usize> {
        wide(self.start.byte_index)..wide(self.start.byte_index + self.length)
    }
}

/// A replica's deletion of a set of bytes, in CRDT coordinates: the exact ids
/// of the deleted bytes, as runs of temporally-consecutive ids.
/// Note [Deletion by id]
#[derive(Clone)]
pub struct Deletion {
    targets: Vec<Run>,
}

impl Deletion {
    /// The earliest intra-span byte range covered by any of this deletion's
    /// target runs, if any.
    fn earliest_overlap(&self, span: &Span) -> Option<(u32, u32)> {
        let mut earliest: Option<(u32, u32)> = None;
        for run in &self.targets {
            if run.start.replica != span.id.replica {
                continue;
            }
            let from = run.start.byte_index.max(span.id.byte_index);
            let to = (run.start.byte_index + run.length).min(span.id.byte_index + span.length);
            if from < to {
                let overlap = (from - span.id.byte_index, to - span.id.byte_index);
                if earliest.is_none_or(|(start, _)| overlap.0 < start) {
                    earliest = Some(overlap);
                }
            }
        }
        earliest
    }
}

/// A run of bytes inserted consecutively by one replica.
#[derive(Clone, Copy)]
struct Run {
    start: ByteId,
    length: u32,
}

impl Run {
    /// The id one past this run, i.e. where a continuation would start.
    fn end(&self) -> ByteId {
        self.start.plus(self.length)
    }

    fn last(&self) -> ByteId {
        self.start.plus(self.length - 1)
    }
}

/// An edit to apply to the text buffer paired with a [`Replica`]. Produced by
/// the `integrate_*` methods; apply edits in the order given.
#[derive(Debug)]
pub enum BufferEdit {
    /// Insert the text of the insertion identified by `author` and `bytes` at
    /// this visible gap position.
    Insert {
        byte_offset: usize,
        author: ReplicaId,
        bytes: Range<usize>,
    },
    /// Remove these visible byte ranges, which are ascending and expressed in
    /// pre-deletion coordinates: apply them back to front.
    Delete { byte_ranges: Vec<Range<usize>> },
}

// The `NonZero` gives `Option<ByteId>` its niche, halving every origin field.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Debug)]
pub struct ReplicaId(NonZeroU32);

#[derive(Clone, Copy, PartialEq)]
struct ByteId {
    replica: ReplicaId,
    /// The value of the author's byte clock when the byte was inserted —
    /// temporal, nothing to do with document position. Note [Spans]
    byte_index: u32,
}

impl ByteId {
    fn plus(self, bytes: u32) -> Self {
        Self {
            replica: self.replica,
            byte_index: self.byte_index + bytes,
        }
    }
}

/// A run of contiguously authored document bytes (the caller knows which text
/// they stand for; the replica never does). Note [Spans]
#[derive(Clone)]
struct Span {
    /// The first byte's id.
    id: ByteId,
    /// Always at least 1.
    length: u32,
    /// The origin of the first byte; interior bytes implicitly chain to their
    /// predecessor.
    origin_left: Option<ByteId>,
    /// The origin shared by every byte in the span.
    origin_right: Option<ByteId>,
    /// Whole-span: spans split before partial deletion.
    deleted: bool,
}

impl Span {
    fn of(op: &Insertion) -> Self {
        Self {
            id: op.start,
            length: op.length,
            origin_left: op.origin_left,
            origin_right: op.origin_right,
            deleted: false,
        }
    }

    fn contains(&self, id: ByteId) -> bool {
        self.id.replica == id.replica
            && (self.id.byte_index..self.id.byte_index + self.length).contains(&id.byte_index)
    }

    /// The id one past this span's last byte, i.e. where a continuation
    /// starts.
    fn end_id(&self) -> ByteId {
        self.id.plus(self.length)
    }

    fn last_id(&self) -> ByteId {
        self.id.plus(self.length - 1)
    }

    fn visible_length(&self) -> usize {
        if self.deleted { 0 } else { wide(self.length) }
    }

    /// Splits off and returns everything from `at` bytes in. Lossless: the
    /// right half's derived origins are exactly the implicit origins its bytes
    /// already had. Note [Spans]
    fn split(&mut self, at: u32) -> Self {
        debug_assert!(0 < at && at < self.length);
        let right = Self {
            id: self.id.plus(at),
            length: self.length - at,
            origin_left: Some(self.id.plus(at - 1)),
            origin_right: self.origin_right,
            deleted: self.deleted,
        };
        self.length = at;
        right
    }

    /// Whether the op directly continues this span, i.e. appending it is
    /// exactly what a later split would undo. Note [Spans]
    fn can_append(&self, op: &Insertion) -> bool {
        !self.deleted
            && self.end_id() == op.start
            && op.origin_left == Some(self.last_id())
            && op.origin_right == self.origin_right
    }
}

// Storage-dominant type sizes, asserted so layout changes are deliberate.
const _: () = assert!(size_of::<ByteId>() == 8);
const _: () = assert!(size_of::<Option<ByteId>>() == 8);
const _: () = assert!(size_of::<Span>() == 32);
const _: () = assert!(size_of::<Insertion>() == 28);
const _: () = assert!(size_of::<Run>() == 12);

/// How many bytes this replica has seen from each other replica. Bytes from
/// one author are totally ordered, so a count fully describes which of its
/// bytes we've seen. A vec, not a map: it holds one entry per replica ever
/// seen, i.e. a handful.
#[derive(Clone, Default)]
struct Version(Vec<(ReplicaId, u32)>);

impl Version {
    fn count(&self, replica: ReplicaId) -> u32 {
        self.0
            .iter()
            .find(|(counted, _)| *counted == replica)
            .map_or(0, |(_, count)| *count)
    }

    fn contains(&self, id: ByteId) -> bool {
        id.byte_index < self.count(id.replica)
    }

    /// Bytes from an author must be recorded in order, without gaps.
    fn record(&mut self, start: ByteId, length: u32) {
        assert_eq!(
            start.byte_index,
            self.count(start.replica),
            "bytes must be recorded in order",
        );
        let count = start
            .byte_index
            .checked_add(length)
            .expect("the byte clock fits in u32");
        match self
            .0
            .iter_mut()
            .find(|(counted, _)| *counted == start.replica)
        {
            Some((_, existing)) => *existing = count,
            None => self.0.push((start.replica, count)),
        }
    }
}

/*
Note [Spans]
------------

Claude: This replica measures the document in bytes. It never sees text — it
only counts bytes and never inspects one. That's what decouples it from the
text: the caller owns the buffer (a `String`, a rope, ...) and the replica
answers *where*, never *what*. It also means the replica can't police char
boundaries; positions are the caller's responsibility, and every replica of a
document must of course index the same byte sequence.

Storage is one `Span` per run of contiguously authored bytes — the same shape
the ops have. A span of n bytes stands, definitionally, for n chained per-byte
items: interior byte k implicitly has byte k-1 as its left origin and the
span's shared right origin as its right. Only the first byte's origins are
stored; the rest are derivable.

Two operations move between coarser and finer runs, and both are lossless.
`split` materializes a chain boundary: the right half's derived id and origins
are exactly the implicit values its bytes already had, so nothing about
identity or ordering changes — which is why replicas may fragment the same
content differently (one split where another appended) and still agree on the
byte sequence. `can_append` is split's inverse: its conditions — temporal
continuation, chained left origin, same right origin — are precisely what
makes a later split recover the original spans byte-for-byte.

The integration scan can judge whole spans by their first byte alone: a
decision made at the head extends to the span's tail, because every interior
byte's left origin points at its in-span predecessor, which lies past any left
anchor the scan could be comparing against — so interior bytes are always
skipped and never flip the scan's state. (The same fact is why a whole op
integrates with a single scan.)

Byte granularity of identity is also why this module converges to the same
visible sequence as the per-char [`crate::crdt_vibed`]: each char item there
corresponds to a run of byte ids here with order-isomorphic origins, and the
FugueMax comparisons only consult relative order and replica ids, which agree.

Note [Integration scan]
-----------------------

Claude: `apply_insertion` places a new span among its rivals: spans inserted
concurrently into the same gap, between the same left and right origins. The
anchors are first split to span boundaries (`split_after` the left origin,
`split_before` the right), then the scan starts just after the left anchor's
span and walks forward, stopping at the latest by the right anchor's span (or
the end of the document). `destination` trails the walk, marking where the
span would go if the scan stopped now.

Rival spans are judged by their first byte's origins — Note [Spans] explains
why the head speaks for the whole span. Origins are mapped to *expansion
coordinates* — the position a byte would have if every span were expanded to
one item per byte — so they compare globally even when an origin points into
the middle of some span elsewhere. A left origin of `None` is the virtual item
before the document, and `Option`'s derived ordering conveniently puts `None`
first, just like the reference implementation's `-1` sentinel. A rival whose
left origin is earlier than ours closes the gap we were inserted into: we
belong before it. One whose left origin is later sits inside an earlier
rival's subtree: we skip past it. Rivals sharing our left origin are true
siblings, ordered by their right origins (later first), with ties broken by
replica id, smaller first.

A sibling whose right origin is earlier than ours might come before or after
us depending on what the scan finds next, so the walk enters a "scanning"
state that leaves `destination` pinned behind: if the scan later decides the
span belongs before the ambiguous run, it splices at the pinned spot, and if
it instead finds a sibling proving the span belongs after, `destination`
catches back up.

Once placed, the span appends onto its left neighbor when it directly
continues it (the common typing case), keeping runs coalesced on every
replica. Local insertions skip all of this: a fresh op's origins are its
physical neighbors, so `inserted` splices — or appends — directly at the gap
the caller named.

Note [Deletion by id]
---------------------

Claude: A `Deletion` names the exact byte ids it deleted (cola instead sends
two anchors plus a version map, a compressed encoding of the same information).
Naming ids makes the two subtle parts of deletion trivial. Bytes inserted
concurrently *inside* the deleted range survive automatically — they aren't in
the list. And integration is idempotent by construction — `apply_deletion`
reports only bytes that this call flipped from visible to deleted, so replaying
a deletion, or receiving two concurrent overlapping deletions, deletes nothing
twice and never mis-reports a buffer range. That idempotence is also why
deletions need none of the ordering machinery insertions have (no timestamps,
no per-author sequencing): a deletion is ready as soon as its targets exist.

Targets may cover only part of a span; the span splits at the overlap's edges
first, so tombstones are always whole spans.

Note [Pending ops]
------------------

Claude: An op can arrive before what it depends on — its author's earlier
bytes, or the bytes its origins name (network reordering). Such ops wait in
`pending_insertions`/`pending_deletions`, invisibly to the caller: whenever an
insertion applies, `drain_pending` re-checks the queues and applies everything
newly ready, and the unblocked edits ride out in the same return value as the
op that unblocked them.

Only insertions advance the version, and readiness only consults the version,
so nothing can become ready after a deletion — `integrate_deletion` doesn't
drain. Duplicate deliveries of a waiting op pile up in the queue; each drain
pass first drops copies whose op has since been applied.
*/

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::generators as gs;
    use std::collections::{HashMap, HashSet};

    fn rid(id: usize) -> NonZeroU32 {
        NonZeroU32::new(u32::try_from(id).expect("test id fits")).expect("nonzero test id")
    }

    /// Harness-level op identity: (author, author-local op number). Lets
    /// `sync_from` skip already-known ops without the CRDT needing deletion
    /// timestamps.
    type OpId = (usize, usize);

    #[derive(Clone)]
    enum Op {
        Insert(Insertion, String),
        Delete(Deletion),
    }

    /// How an application drives a [`Replica`] (cola's README pattern): the
    /// text lives outside the CRDT — here in a `String`.
    struct Doc {
        id: usize,
        buffer: String,
        replica: Replica,
        /// Every op this doc knows, own and received, in a causally valid
        /// order (its own integration order).
        history: Vec<(OpId, Op)>,
        known: HashSet<OpId>,
        /// Texts of known insertions, for resolving `BufferEdit::Insert`s.
        texts: HashMap<(ReplicaId, usize), String>,
        authored: usize,
    }

    impl Doc {
        fn new(id: usize) -> Self {
            Self {
                id,
                buffer: String::new(),
                replica: Replica::new(rid(id), 0),
                history: Vec::new(),
                known: HashSet::new(),
                texts: HashMap::new(),
                authored: 0,
            }
        }

        fn insert(&mut self, byte_offset: usize, text: &str) {
            self.buffer.insert_str(byte_offset, text);
            let op = self.replica.inserted(byte_offset, text.len()).unwrap();
            self.record(Op::Insert(op, text.to_owned()));
        }

        fn delete(&mut self, byte_range: Range<usize>) {
            let op = self.replica.deleted(byte_range.clone()).unwrap();
            self.buffer.replace_range(byte_range, "");
            self.record(Op::Delete(op));
        }

        fn record(&mut self, op: Op) {
            let op_id = (self.id, self.authored);
            self.authored += 1;
            self.known.insert(op_id);
            self.stash_text(&op);
            self.history.push((op_id, op));
        }

        /// An empty insertion advances no clock, so it would share its
        /// `bytes().start` with the author's next insertion — and it never
        /// produces an `Insert` edit anyway. Don't let it clobber the map.
        fn stash_text(&mut self, op: &Op) {
            if let Op::Insert(insertion, text) = op
                && !text.is_empty()
            {
                self.texts
                    .insert((insertion.author(), insertion.bytes().start), text.clone());
            }
        }

        fn receive(&mut self, op_id: OpId, op: &Op) {
            if !self.known.insert(op_id) {
                return;
            }
            self.stash_text(op);
            let edits = match op {
                Op::Insert(insertion, _) => self.replica.integrate_insertion(insertion),
                Op::Delete(deletion) => self.replica.integrate_deletion(deletion),
            };
            self.history.push((op_id, op.clone()));
            self.apply(edits);
        }

        fn apply(&mut self, edits: Vec<BufferEdit>) {
            for edit in edits {
                match edit {
                    BufferEdit::Insert {
                        byte_offset,
                        author,
                        bytes,
                    } => {
                        let text = &self.texts[&(author, bytes.start)];
                        self.buffer.insert_str(byte_offset, text);
                    }
                    BufferEdit::Delete { byte_ranges } => {
                        for byte_range in byte_ranges.into_iter().rev() {
                            self.buffer.replace_range(byte_range, "");
                        }
                    }
                }
            }
        }

        fn sync_from(&mut self, other: &Self) {
            for (op_id, op) in &other.history {
                self.receive(*op_id, op);
            }
        }
    }

    /// Every byte's author, byte index, and tombstone flag, in document order
    /// — spans expanded per byte, normalizing fragmentation differences
    /// between replicas.
    fn items(replica: &Replica) -> Vec<(u32, u32, bool)> {
        replica
            .spans
            .iter()
            .flat_map(|span| {
                (span.id.byte_index..span.id.byte_index + span.length)
                    .map(move |byte_index| (span.id.replica.0.get(), byte_index, span.deleted))
            })
            .collect()
    }

    /// Draws a byte index in `[0, text.len()]` lying on a `char` boundary.
    fn draw_boundary(tc: &hegel::TestCase, text: &str) -> usize {
        let mut index = tc.draw(gs::integers::<usize>().max_value(text.len()));
        while !text.is_char_boundary(index) {
            index += 1;
        }
        index
    }

    fn draw_text(tc: &hegel::TestCase, ascii: bool) -> String {
        if ascii {
            tc.draw(gs::text().codec("ascii"))
        } else {
            tc.draw(gs::text())
        }
    }

    fn edit_randomly(tc: &hegel::TestCase, doc: &mut Doc) {
        if doc.buffer.is_empty() || tc.draw(gs::booleans()) {
            let byte_offset = draw_boundary(tc, &doc.buffer);
            let text = draw_text(tc, false);
            doc.insert(byte_offset, &text);
        } else {
            let one = draw_boundary(tc, &doc.buffer);
            let two = draw_boundary(tc, &doc.buffer);
            doc.delete(one.min(two)..one.max(two));
        }
    }

    fn edit_many(tc: &hegel::TestCase, doc: &mut Doc, up_to: usize) {
        let edits = tc.draw(gs::integers::<usize>().max_value(up_to));
        for _ in 0..edits {
            edit_randomly(tc, doc);
        }
    }

    /// `docs[into].sync_from(&docs[from])`, which the borrow checker only
    /// allows through a split.
    fn sync_pair(docs: &mut [Doc], into: usize, from: usize) {
        if into < from {
            let (left, right) = docs.split_at_mut(from);
            left[into].sync_from(&right[0]);
        } else {
            let (left, right) = docs.split_at_mut(into);
            right[0].sync_from(&left[from]);
        }
    }

    #[test]
    fn ties_broken_by_smaller_replica_id() {
        let mut doc1 = Doc::new(1);
        let mut doc2 = Doc::new(2);

        doc1.insert(0, "A");
        doc2.insert(0, "B");

        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);

        assert_eq!(doc1.buffer, "AB");
        assert_eq!(doc2.buffer, "AB");
    }

    #[test]
    fn initial_content_and_fork() {
        let mut replica1 = Replica::new(rid(1), 5);
        let mut replica2 = replica1.fork(rid(2));
        assert_eq!(replica2.len(), 5);

        let op = replica2.inserted(5, 2).unwrap();
        let edits = replica1.integrate_insertion(&op);
        assert!(matches!(
            edits.as_slice(),
            [BufferEdit::Insert { byte_offset: 5, .. }]
        ));
        assert_eq!(replica1.len(), 7);
    }

    #[test]
    fn invalid_positions_are_errors() {
        let mut replica = Replica::new(rid(1), 3);
        assert!(replica.inserted(4, 1).is_err());
        assert!(replica.deleted(1..5).is_err());
        let backwards = (2, 1);
        assert!(replica.deleted(backwards.0..backwards.1).is_err());
        assert_eq!(replica.len(), 3);
    }

    /// The whole point of spans: runs of typing coalesce into one span, both
    /// for the author and for a replica integrating the keystrokes one op at
    /// a time.
    #[test]
    fn typing_coalesces_into_one_span() {
        let mut doc1 = Doc::new(1);
        for (byte_offset, char) in "hello world".char_indices() {
            doc1.insert(byte_offset, &char.to_string());
        }
        assert_eq!(doc1.replica.spans.len(), 1);

        let mut doc2 = Doc::new(2);
        doc2.sync_from(&doc1);
        assert_eq!(doc2.replica.spans.len(), 1);
    }

    // Canonical FugueMax scenarios: text typed concurrently at one position
    // must never interleave, whether typed forward or backward (the backward
    // case is the anomaly FugueMax exists to fix).

    #[test]
    fn forward_typing_does_not_interleave() {
        let mut doc1 = Doc::new(1);
        let mut doc2 = Doc::new(2);
        for (byte_offset, char) in "Hello".char_indices() {
            doc1.insert(byte_offset, &char.to_string());
        }
        for (byte_offset, char) in "World".char_indices() {
            doc2.insert(byte_offset, &char.to_string());
        }
        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        assert_eq!(doc1.buffer, "HelloWorld");
        assert_eq!(doc2.buffer, "HelloWorld");
    }

    #[test]
    fn backward_typing_does_not_interleave() {
        let mut doc1 = Doc::new(1);
        let mut doc2 = Doc::new(2);
        for char in "Hello".chars().rev() {
            doc1.insert(0, &char.to_string());
        }
        for char in "World".chars().rev() {
            doc2.insert(0, &char.to_string());
        }
        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        assert_eq!(doc1.buffer, doc2.buffer);
        assert!(doc1.buffer.contains("Hello"));
        assert!(doc1.buffer.contains("World"));
    }

    #[test]
    fn mixed_typing_does_not_interleave() {
        let mut doc1 = Doc::new(1);
        let mut doc2 = Doc::new(2);
        for (byte_offset, char) in "Hello".char_indices() {
            doc1.insert(byte_offset, &char.to_string());
        }
        for char in "World".chars().rev() {
            doc2.insert(0, &char.to_string());
        }
        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        assert_eq!(doc1.buffer, doc2.buffer);
        assert!(doc1.buffer.contains("Hello"));
        assert!(doc1.buffer.contains("World"));
    }

    /// Types `text` into the document one char-sized op at a time, either
    /// forward or backward.
    fn type_at(tc: &hegel::TestCase, doc: &mut Doc, byte_offset: usize, text: &str) {
        if tc.draw(gs::booleans()) {
            let mut byte_offset = byte_offset;
            for char in text.chars() {
                doc.insert(byte_offset, &char.to_string());
                byte_offset += char.len_utf8();
            }
        } else {
            for char in text.chars().rev() {
                doc.insert(byte_offset, &char.to_string());
            }
        }
    }

    /// The property version of the scenarios above; the disjoint alphabets
    /// make the substring checks exact.
    #[hegel::test(test_cases = 2_000)]
    fn concurrent_typing_does_not_interleave(tc: hegel::TestCase) {
        let base: String = tc.draw(gs::text().alphabet("xyz"));
        let mut doc1 = Doc::new(1);
        doc1.insert(0, &base);
        let mut doc2 = Doc::new(2);
        doc2.sync_from(&doc1);

        let byte_offset = draw_boundary(&tc, &base);
        let text1: String = tc.draw(gs::text().alphabet("ABC").min_size(1));
        let text2: String = tc.draw(gs::text().alphabet("123").min_size(1));
        type_at(&tc, &mut doc1, byte_offset, &text1);
        type_at(&tc, &mut doc2, byte_offset, &text2);

        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        assert_eq!(doc1.buffer, doc2.buffer);
        assert!(doc1.buffer.contains(&text1));
        assert!(doc1.buffer.contains(&text2));
    }

    /// doc1 types a run, doc2 integrates it, then both continue typing at the
    /// same position: doc1 extending its own material (the append path), doc2
    /// starting a rival run (the split/sibling path). Neither author's text
    /// may be torn apart.
    #[hegel::test(test_cases = 2_000)]
    fn continued_typing_after_merge_does_not_interleave(tc: hegel::TestCase) {
        let mut doc1 = Doc::new(1);
        let first: String = tc.draw(gs::text().alphabet("ABC").min_size(1));
        type_at(&tc, &mut doc1, 0, &first);
        let mut doc2 = Doc::new(2);
        doc2.sync_from(&doc1);

        let byte_offset = draw_boundary(&tc, &doc1.buffer);
        let more: String = tc.draw(gs::text().alphabet("abc").min_size(1));
        let rival: String = tc.draw(gs::text().alphabet("123").min_size(1));
        type_at(&tc, &mut doc1, byte_offset, &more);
        type_at(&tc, &mut doc2, byte_offset, &rival);

        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        assert_eq!(doc1.buffer, doc2.buffer);
        assert_eq!(items(&doc1.replica), items(&doc2.replica));
        assert!(doc1.buffer.contains(&more));
        assert!(doc1.buffer.contains(&rival));
    }

    #[hegel::test(test_cases = 2_000)]
    fn local_edits_match_string(tc: hegel::TestCase) {
        let mut doc = Doc::new(1);
        let edits = tc.draw(gs::integers::<usize>().max_value(20));
        for _ in 0..edits {
            edit_randomly(&tc, &mut doc);
            assert_eq!(doc.replica.len(), doc.buffer.len());
        }

        // A mirror doc reconstructs the text purely from length-based edits.
        let mut mirror = Doc::new(2);
        mirror.sync_from(&doc);
        assert_eq!(mirror.buffer, doc.buffer);
    }

    #[hegel::test(test_cases = 2_000)]
    fn concurrent_edits_converge(tc: hegel::TestCase) {
        let count = tc.draw(gs::integers::<usize>().min_value(2).max_value(4));
        let mut docs = (1..=count).map(Doc::new).collect::<Vec<Doc>>();

        let rounds = tc.draw(gs::integers::<usize>().min_value(1).max_value(3));
        for _ in 0..rounds {
            // Everyone edits concurrently...
            for doc in &mut docs {
                edit_many(&tc, doc, 3);
            }
            // ...then two docs gossip.
            let into = tc.draw(gs::integers::<usize>().max_value(count - 1));
            let from = tc.draw(gs::integers::<usize>().max_value(count - 1));
            if into != from {
                sync_pair(&mut docs, into, from);
            }
        }

        // After syncing all-to-all, every doc has seen every op.
        for into in 0..count {
            for from in 0..count {
                if into != from {
                    sync_pair(&mut docs, into, from);
                }
            }
        }
        for pair in docs.windows(2) {
            assert_eq!(pair[0].buffer, pair[1].buffer);
            assert_eq!(items(&pair[0].replica), items(&pair[1].replica));
        }
    }

    #[hegel::test(test_cases = 2_000)]
    fn out_of_order_delivery(tc: hegel::TestCase) {
        let mut doc1 = Doc::new(1);
        edit_many(&tc, &mut doc1, 6);

        // Deliver doc1's ops in a random order; the pending queues absorb the
        // reordering invisibly.
        let mut doc2 = Doc::new(2);
        let mut remaining = (0..doc1.history.len()).collect::<Vec<usize>>();
        while !remaining.is_empty() {
            let pick = tc.draw(gs::integers::<usize>().max_value(remaining.len() - 1));
            let index = remaining.swap_remove(pick);
            let (op_id, op) = &doc1.history[index];
            doc2.receive(*op_id, op);
        }

        assert_eq!(doc2.buffer, doc1.buffer);
        assert!(doc2.replica.pending_insertions.is_empty());
        assert!(doc2.replica.pending_deletions.is_empty());
    }

    #[hegel::test(test_cases = 2_000)]
    fn integration_is_idempotent(tc: hegel::TestCase) {
        let mut doc1 = Doc::new(1);
        let mut doc2 = Doc::new(2);
        edit_many(&tc, &mut doc1, 4);
        edit_many(&tc, &mut doc2, 4);

        // Share some history, then diverge again, so replays hit partially
        // overlapping versions rather than a wholly foreign doc.
        doc1.sync_from(&doc2);
        doc2.sync_from(&doc1);
        edit_many(&tc, &mut doc1, 2);
        doc2.sync_from(&doc1);

        let buffer = doc2.buffer.clone();
        let snapshot = items(&doc2.replica);
        for (_, op) in &doc1.history {
            let edits = match op {
                Op::Insert(insertion, _) => doc2.replica.integrate_insertion(insertion),
                Op::Delete(deletion) => doc2.replica.integrate_deletion(deletion),
            };
            assert!(edits.is_empty());
        }
        assert_eq!(doc2.buffer, buffer);
        assert_eq!(items(&doc2.replica), snapshot);
    }

    /// The headline requirement: driven by the same edits and the same merge
    /// schedule, this module and the per-char `crdt_vibed` implementation
    /// produce identical documents. In ASCII mode (1 char == 1 byte) the full
    /// internal item sequences must match too, not just the visible text.
    #[hegel::test(test_cases = 2_000)]
    fn matches_char_based_implementation(tc: hegel::TestCase) {
        use crate::crdt_vibed;

        fn merge_pair_old(docs: &mut [crdt_vibed::Document], into: usize, from: usize) {
            if into < from {
                let (left, right) = docs.split_at_mut(from);
                left[into].merge(&right[0]);
            } else {
                let (left, right) = docs.split_at_mut(into);
                right[0].merge(&left[from]);
            }
        }

        let ascii = tc.draw(gs::booleans());
        let count = tc.draw(gs::integers::<usize>().min_value(2).max_value(3));
        let mut olds = (1..=count)
            .map(crdt_vibed::Document::new)
            .collect::<Vec<crdt_vibed::Document>>();
        let mut news = (1..=count).map(Doc::new).collect::<Vec<Doc>>();

        let rounds = tc.draw(gs::integers::<usize>().min_value(1).max_value(3));
        for _ in 0..rounds {
            for index in 0..count {
                let edits = tc.draw(gs::integers::<usize>().max_value(3));
                for _ in 0..edits {
                    let old = &mut olds[index];
                    let new = &mut news[index];
                    if new.buffer.is_empty() || tc.draw(gs::booleans()) {
                        let byte_offset = draw_boundary(&tc, &new.buffer);
                        let text = draw_text(&tc, ascii);
                        old.insert(byte_offset, &text).unwrap();
                        new.insert(byte_offset, &text);
                    } else {
                        let one = draw_boundary(&tc, &new.buffer);
                        let two = draw_boundary(&tc, &new.buffer);
                        let byte_range = one.min(two)..one.max(two);
                        old.delete(byte_range.clone()).unwrap();
                        new.delete(byte_range);
                    }
                }
            }
            let into = tc.draw(gs::integers::<usize>().max_value(count - 1));
            let from = tc.draw(gs::integers::<usize>().max_value(count - 1));
            if into != from {
                merge_pair_old(&mut olds, into, from);
                sync_pair(&mut news, into, from);
                assert_eq!(olds[into].text(), news[into].buffer);
            }
        }

        for into in 0..count {
            for from in 0..count {
                if into != from {
                    merge_pair_old(&mut olds, into, from);
                    sync_pair(&mut news, into, from);
                }
            }
        }
        for index in 0..count {
            assert_eq!(olds[index].text(), news[index].buffer);
            if ascii {
                let old_items = olds[index]
                    .item_summaries()
                    .into_iter()
                    .map(|(replica, sequence, deleted)| {
                        (
                            u32::try_from(replica).unwrap(),
                            u32::try_from(sequence).unwrap(),
                            deleted,
                        )
                    })
                    .collect::<Vec<(u32, u32, bool)>>();
                assert_eq!(old_items, items(&news[index].replica));
            }
        }
    }
}
