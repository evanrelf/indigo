# Guidance for agents

## About

[Indigo] is a work-in-progress, terminal-native modal code editor, inspired by
[Kakoune].

[Indigo]: https://github.com/evanrelf/indigo
[Kakoune]: https://github.com/mawww/kakoune

## Project structure

Indigo is written in Rust, across multiple crates in a Cargo workspace:

- **indigo-core:** Core types and functions, defines editor behavior. Thick
  functional core, performs no I/O.

- **indigo-tui:** Interactive TUI, primary frontend. Thin imperative shell.

- **indigo-cli:** Headless CLI, secondary frontend. Thin imperative shell.

- **indigo-wrap:** Limited form of higher-kinded types, provides generic wrapper
  types and "mutability generics". Powers `indigo-core`'s "view types."

## Checking your work

- Always run `cargo clippy` to check if code compiles. Never run `cargo check`
  or `cargo build`.

- Never run `indigo-tui`; you cannot control it. Always check runtime behavior
  by writing tests or executables at `crates/<crate>/src/bin/<name>.rs`. Always
  run these ad-hoc executables with `cargo run --bin <name>`, never with `rustc`
  (no dependencies outside of Cargo workspace).

- Run `indigo-cli` to test editor behavior. With the `--debug` flag, the `<c-l>`
  key prints information and a diff. Try running this command as an example:

  ```
  $ echo -ne "hello\nworld\n" | ./bin/cli --debug 'fod<c-l>iHELLO<c-l><esc>jgla!<c-l>'
  ```
