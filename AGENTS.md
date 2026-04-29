# Guidance for coding agents

## Project structure

Indigo is written in Rust, across multiple crates in a Cargo workspace. The
primary crates are the core, and the two frontends (interactive TUI and headless
CLI):

- [indigo-core](crates/indigo-core/README.md): the functional core, making up
  the majority of code.
- [indigo](crates/indigo/README.md): interactive TUI, the main application that
  hooks up the core to an event loop (user input -> core -> render to terminal).
- [indigo-cli](crates/indigo-cli/README.md): headless CLI, currently used for
  debugging. Read the readme for instructions on how to use it (it's basically
  an agent skill).

And then there are supporting crates that are either currently unused,
infrequently changed, or feature complete:

- [indigo-llm](crates/indigo-llm/README.md): talk to 3rd party LLM APIs,
  currently unused.
- [indigo-term](crates/indigo-term/README.md): low-level terminal manipulation
  library, currently unused.
- [indigo-tui](crates/indigo-tui/README.md): high-level terminal UI library,
  currently unused.
- [indigo-wrap](crates/indigo-wrap/README.md): higher-kinded types, mutability
  generics, feature complete, powers `{Cursor,Range,Selection,Window}View` types
  found in the core.

## Checking your work

- Always run `cargo clippy --all-targets` to check if code compiles. Never run
  `cargo check` or `cargo build`.
- Never run `indigo`; you cannot control it. Run `indigo-cli` instead to test
  editor behavior.
