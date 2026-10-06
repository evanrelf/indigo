# Differences from Kakoune

This list is not exhaustive!

- Kakoune moves by Unicode code point. Indigo moves by Unicode grapheme cluster.

  Reason: I care a lot about correct manipulation and display of text, and I
  think working in terms of graphemes is the right choice. If you need to
  manipulate code points or bytes, use a different tool.

- Kakoune uses `b` for backwards word movement. Indigo uses `q`.

  Reason: It's near+behind `w` and `e`.

- Kakoune preserves selection ranges in insert mode. Indigo collapses (`;`) them
  to a single grapheme when entering.

  Reason: Cursors are easier (for me) to reason about than ranges when
  inserting. I may reconsider this later.

- Kakoune has a separate "goto (extend to)" mode (`G`). Indigo bundles extend
  motions under the regular "goto" mode (`g`). For example, `Gl` in Kakoune
  becomes `gL` in Indigo.

  Reason: I don't feel another mode is necessary.

- Kakoune and Indigo have different syntaxes for keys. Read `key.rs` in
  `indigo-core` or try the `indigo-parse-keys` bin from `indigo-cli` to learn
  more.

  Reason: I don't care about retaining compatibility here. And by requiring you
  escape spaces (` `) and hashes (`#`), I can support multi-line key sequences
  with comments. This is inspired by the [`regex` crate's "verbose mode"][1].

- Kakoune supports editing arbitrarily large(?) files. Indigo has a hard 4 GiB
  cap.

  Reason: By limiting to 4 GiB, I can use `u32` as an index for bytes in the
  text rather than `u64`/`usize`. If you need to edit text files 4 GiB or
  larger, ~~I am worried for you~~ use a different tool.

[1]: https://docs.rs/regex/latest/regex/#example-verbose-mode
