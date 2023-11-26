# Indigo

A text editor

## Todo

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
