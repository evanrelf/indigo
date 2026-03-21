# Guidance for coding agents

## About

[Indigo] is a work-in-progress, terminal-native modal code editor, inspired by
[Kakoune].

[Indigo]: https://github.com/evanrelf/indigo
[Kakoune]: https://github.com/mawww/kakoune

## Project structure

Indigo is written in Rust, across multiple crates in a Cargo workspace.

```
$ find crates/ -name README.md | sed 's/^/@/'
@crates/indigo-core/README.md
@crates/indigo-llm/README.md
@crates/indigo-cli/README.md
@crates/indigo-term/README.md
@crates/indigo-tui/README.md
@crates/indigo/README.md
@crates/indigo-wrap/README.md
```

## Checking your work

- Always run `cargo clippy` to check if code compiles. Never run `cargo check`
  or `cargo build`.

- Never run `indigo`; you cannot control it. Always check runtime behavior by
  writing tests or executables at `crates/<crate>/src/bin/<name>.rs`. Always run
  these ad-hoc executables with `cargo run --bin <name>`, never with `rustc` (no
  dependencies outside of Cargo workspace).

- Run `indigo-cli` to test editor behavior. With the `--debug` flag, the `<c-l>`
  key prints information and a diff. Try running this command as an example:

  ```
  $ echo -ne "hello\nworld\n" | ./bin/run-cli --debug 'fod<c-l>iHELLO<c-l><esc>jgla!<c-l>'
  ```
