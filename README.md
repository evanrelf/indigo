# Indigo

A text editor

## Todo

- **Fix panic when inserting newlines.**

  Panic message:

  ```
  thread 'main' panicked at indigo-core/src/selection.rs:118:14:
  insert never fails because selection positions are always valid: Position isn't valid in rope
  ```

  Reproduction:

  1. Run `echo -ne "\n" | pbcopy`.
  2. Run `cargo run --bin indigo-tui -- (echo "foo" | psub)`.
  3. Press `i` to enter insert mode, then paste twice.

- **Handle `<enter>` key in insert mode.**

- **Handle `<backspace>` key in insert mode.**

- **Move key event handling to `indigo-core`.**

  Most behavior is frontend-agnostic, and should be shared. Frontend-specific
  behavior can layer atop the core. This will be gross at first, but should
  motivate developing a more ergonomic keymap abstraction.

## Notes

- I wonder where `Window` and/or `{vertical,horizontal}_scroll` should live.
  Like, what should `indigo-cli` do with `<c-d>`? I guess it depends on whether
  Indigo ever supports viewport-dependent operations, like Vim's `H`/`M`/`L` and
  others. If I can avoid them that sounds more principled, but it might not be
  possible.
