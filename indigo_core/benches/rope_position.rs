use criterion::{black_box, criterion_group, criterion_main, Criterion};
use indigo_core::position::Position;
use once_cell::sync::Lazy;
use ropey::Rope;
use std::fs::File;

static ROPE: Lazy<Rope> = Lazy::new(|| {
    let file = File::open("../Cargo.lock").unwrap();
    Rope::from_reader(file).unwrap()
});

static POSITION: Lazy<Position> = Lazy::new(|| {
    let line = ROPE.len_lines() / 2;
    let column = 1;
    Position { line, column }
});

static INDEX: Lazy<usize> = Lazy::new(|| POSITION.to_rope_index(&ROPE));

fn to_rope_index(criterion: &mut Criterion) {
    let position = Lazy::force(&POSITION);
    let rope = Lazy::force(&ROPE);

    criterion.bench_function("to_rope_index", |bencher| {
        bencher.iter(|| black_box(position).to_rope_index(black_box(rope)))
    });
}

fn from_rope_index(criterion: &mut Criterion) {
    let rope = Lazy::force(&ROPE);
    let index = *Lazy::force(&INDEX);

    criterion.bench_function("from_rope_index", |bencher| {
        bencher.iter(|| Position::from_rope_index(black_box(rope), black_box(index)))
    });
}

criterion_group!(benches, to_rope_index, from_rope_index);
criterion_main!(benches);
