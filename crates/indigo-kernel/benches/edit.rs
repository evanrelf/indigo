//! Compares `edit` and `edit2` on realistic worst cases: canonical-form churn from interleaved
//! inserts and deletes, and mass-cursor edits (a cursor on every word of a multi-megabyte
//! document, à la Kakoune's select-every-match).

use divan::Bencher;
use indigo_kernel::{edit, edit2};
use ropey::Rope;

fn main() {
    divan::main();
}

/// The API shared by `edit::Edit` and `edit2::Edit`, so each benchmark is written once and run
/// against both. `forward` selects `Bias::Forward` over `Bias::Backward`.
trait Ot: Clone + Default {
    fn retain(&mut self, byte_length: usize);
    fn delete(&mut self, text: &str);
    fn insert(&mut self, text: &str);
    fn compose(&self, other: &Self) -> Self;
    fn invert(&self) -> Self;
    fn rebase(&self, onto: &Self, forward: bool) -> Self;
    fn transform_byte_indexes(&self, byte_indexes: &mut [usize], forward: bool);
    fn apply(&self, rope: &mut Rope);
}

macro_rules! impl_ot {
    ($module:ident) => {
        impl Ot for $module::Edit {
            fn retain(&mut self, byte_length: usize) {
                $module::Edit::retain(self, byte_length);
            }

            fn delete(&mut self, text: &str) {
                $module::Edit::delete(self, text);
            }

            fn insert(&mut self, text: &str) {
                $module::Edit::insert(self, text);
            }

            fn compose(&self, other: &Self) -> Self {
                $module::Edit::compose(self, other).unwrap()
            }

            fn invert(&self) -> Self {
                $module::Edit::invert(self)
            }

            fn rebase(&self, onto: &Self, forward: bool) -> Self {
                let bias = if forward {
                    $module::Bias::Forward
                } else {
                    $module::Bias::Backward
                };
                $module::Edit::rebase(self, onto, bias).unwrap()
            }

            fn transform_byte_indexes(&self, byte_indexes: &mut [usize], forward: bool) {
                let bias = if forward {
                    $module::Bias::Forward
                } else {
                    $module::Bias::Backward
                };
                $module::Edit::transform_byte_indexes(self, byte_indexes, bias);
            }

            fn apply(&self, rope: &mut Rope) {
                $module::Edit::apply(self, rope).unwrap();
            }
        }
    };
}

impl_ot!(edit);
impl_ot!(edit2);

/// The document is `WORD` repeated `word_count` times (one cursor per word) followed by a
/// multi-megabyte retained tail.
const WORD: &str = "hello ";
const REPLACEMENT: &str = "goodbye";
const TAIL_LENGTH: usize = 2 * 1024 * 1024;

fn doc(word_count: usize) -> String {
    let mut doc = WORD.repeat(word_count);
    doc.push_str(&"x".repeat(TAIL_LENGTH));
    doc
}

/// A cursor after every word, each inserting one character.
fn multicursor_insert<E: Ot>(word_count: usize) -> E {
    let mut edit = E::default();
    for _ in 0..word_count {
        edit.retain(WORD.len());
        edit.insert("y");
    }
    edit.retain(TAIL_LENGTH);
    edit
}

/// A cursor on every word, each replacing it.
fn multicursor_replace<E: Ot>(word_count: usize) -> E {
    let mut edit = E::default();
    for _ in 0..word_count {
        edit.delete(WORD.trim_end());
        edit.insert(REPLACEMENT);
        edit.retain(1);
    }
    edit.retain(TAIL_LENGTH);
    edit
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn build_multicursor_insert<E: Ot>(bencher: Bencher, word_count: usize) {
    bencher.bench_local(|| multicursor_insert::<E>(word_count));
}

/// Like `multicursor_replace`, but inserting before deleting, so every delete lands after an
/// insert and takes the canonical-form reordering path.
#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn build_pathological_insert_then_delete<E: Ot>(bencher: Bencher, word_count: usize) {
    bencher.bench_local(|| {
        let mut edit = E::default();
        for _ in 0..word_count {
            edit.insert(REPLACEMENT);
            edit.delete(WORD.trim_end());
            edit.retain(1);
        }
        edit.retain(TAIL_LENGTH);
        edit
    });
}

/// Alternating single-character inserts and deletes at one position; everything merges into a
/// single delete op followed by a single insert op.
#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn build_merge_churn<E: Ot>(bencher: Bencher, op_count: usize) {
    bencher.bench_local(|| {
        let mut edit = E::default();
        for _ in 0..op_count {
            edit.insert("x");
            edit.delete("y");
        }
        edit
    });
}

/// Folding a burst of consecutive single-character keystrokes into one edit, as when coalescing
/// undo history.
#[divan::bench(types = [edit::Edit, edit2::Edit])]
fn compose_typing_burst<E: Ot>(bencher: Bencher) {
    let length = 4 * 1024 * 1024;
    let position = length / 2;
    let edits: Vec<E> = (0..100)
        .map(|i| {
            let mut edit = E::default();
            edit.retain(position + i);
            edit.insert("x");
            edit.retain(length - position);
            edit
        })
        .collect();

    bencher.bench_local(|| {
        let mut composed = edits[0].clone();
        for edit in &edits[1..] {
            composed = composed.compose(edit);
        }
        composed
    });
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn compose_multicursor<E: Ot>(bencher: Bencher, word_count: usize) {
    let a = multicursor_insert::<E>(word_count);
    let mut b = E::default();
    for _ in 0..word_count {
        b.retain(WORD.len() + 1);
        b.insert("z");
    }
    b.retain(TAIL_LENGTH);

    bencher.bench_local(|| a.compose(&b));
}

/// One multi-megabyte paste composed with a multicursor edit over the pasted text, splitting the
/// paste's single insert op into `word_count` pieces.
#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn compose_paste_then_multicursor<E: Ot>(bencher: Bencher, word_count: usize) {
    let mut a = E::default();
    a.insert(&WORD.repeat(word_count));
    let mut b = E::default();
    for _ in 0..word_count {
        b.retain(WORD.len());
        b.insert("z");
    }

    bencher.bench_local(|| a.compose(&b));
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn rebase_multicursor<E: Ot>(bencher: Bencher, word_count: usize) {
    let a = multicursor_insert::<E>(word_count);
    let b = multicursor_insert::<E>(word_count);

    bencher.bench_local(|| (a.rebase(&b, true), a.rebase(&b, false)));
}

/// A multicursor edit rebased onto a concurrent multi-megabyte paste.
#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn rebase_onto_big_paste<E: Ot>(bencher: Bencher, word_count: usize) {
    let a = multicursor_insert::<E>(word_count);
    let mut onto = E::default();
    onto.insert(&"p".repeat(4 * 1024 * 1024));
    onto.retain(WORD.len() * word_count + TAIL_LENGTH);

    bencher.bench_local(|| a.rebase(&onto, false));
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn transform_byte_indexes<E: Ot>(bencher: Bencher, word_count: usize) {
    let edit = multicursor_replace::<E>(word_count);
    let indexes: Vec<usize> = (0..word_count).map(|i| i * WORD.len()).collect();

    bencher
        .with_inputs(|| (indexes.clone(), indexes.clone()))
        .bench_local_values(|(mut forward, mut backward)| {
            edit.transform_byte_indexes(&mut forward, true);
            edit.transform_byte_indexes(&mut backward, false);
            (forward, backward)
        });
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn invert_multicursor<E: Ot>(bencher: Bencher, word_count: usize) {
    let edit = multicursor_replace::<E>(word_count);

    bencher.bench_local(|| edit.invert());
}

#[divan::bench(types = [edit::Edit, edit2::Edit], args = [1_000, 10_000, 100_000])]
fn apply_multicursor<E: Ot>(bencher: Bencher, word_count: usize) {
    let edit = multicursor_replace::<E>(word_count);
    let rope = Rope::from(doc(word_count).as_str());

    bencher
        .with_inputs(|| rope.clone())
        .bench_local_values(|mut rope| {
            edit.apply(&mut rope);
            rope
        });
}
