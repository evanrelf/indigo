// cargo bench --package indigo-kernel --bench display_width

use divan::{Bencher, black_box};
use indigo_kernel::display_width::DisplayWidth as _;
use ropey::Rope;

fn main() {
    divan::main();
}

#[rustfmt::skip]
const CASES: &[(&str, &str)] = &[
    ("grapheme_ascii", "a"),
    ("grapheme_emoji", "👩🏻‍❤️‍💋‍👩🏻"),
    ("line_ascii", "        let width = line.display_width().min(area.width);\n"),
    ("line_tabs", "\t\tlet width = line.display_width().min(area.width);\n"),
    ("line_one_non_ascii", "        // Kakoune → Indigo: measure the line in columns\n"),
    ("line_cjk", "終端モーダルテキストエディタ、主にKakouneとCodeMirrorに触発された。\n"),
    ("line_long_ascii", "long"),
];

fn input(name: &str) -> String {
    let (_, text) = CASES.iter().find(|(case, _)| *case == name).unwrap();
    match name {
        "line_long_ascii" => "let x = 1; ".repeat(90) + "\n",
        _ => (*text).to_owned(),
    }
}

#[divan::bench(args = CASES.iter().map(|(name, _)| *name))]
fn str(bencher: Bencher, name: &str) {
    let text = input(name);
    bencher.bench_local(|| black_box(text.as_str()).display_width());
}

#[divan::bench(args = CASES.iter().map(|(name, _)| *name))]
fn rope_slice(bencher: Bencher, name: &str) {
    let text = input(name);
    let rope = Rope::from_str(
        &"filler line\n"
            .repeat(2_000)
            .replacen("filler line\n", &text, 1),
    );
    let slice = rope.slice(..text.len());
    bencher.bench_local(|| black_box(slice).display_width());
}
