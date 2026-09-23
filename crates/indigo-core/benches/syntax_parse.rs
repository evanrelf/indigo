// cargo bench --package indigo-core --bench syntax_parse --features language-rust

fn main() {
    divan::main();
}

const INSERTIONS: usize = 100;
const LINE: &str = "fn f() { let x = 1; }\n";
const INSERTION_PREFIX: &str = "fn f() { let x";

#[divan::bench(sample_count = 5, args = [100, 1_000, 10_000])]
fn edit_without_syntax(bencher: divan::Bencher, lines: usize) {
    edit(bencher, lines, false);
}

#[divan::bench(sample_count = 5, args = [100, 1_000, 10_000])]
fn edit_with_syntax(bencher: divan::Bencher, lines: usize) {
    edit(bencher, lines, true);
}

fn edit(bencher: divan::Bencher, lines: usize, with_syntax: bool) {
    use indigo_core::{syntax::Language, text::Text};
    use ropey::Rope;

    let source = Rope::from(LINE.repeat(lines));
    let insertion_byte_index = (lines / 2) * LINE.len() + INSERTION_PREFIX.len();

    bencher.bench_local(|| {
        let mut text = Text::from(source.clone());
        if with_syntax {
            text.set_language(Language::Rust);
        }
        for offset in 0..INSERTIONS {
            text.insert(insertion_byte_index + offset, "x")
                .expect("benchmark insertion is valid");
        }
        text.reparse();
        divan::black_box(text);
    });
}
