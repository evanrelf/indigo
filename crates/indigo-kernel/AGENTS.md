@README.md

- Keep this crate deterministic. For example, `Hash{Map,Set}` must use
  `IndigoBuildHasher`. If you notice existing sources of nondeterminism, call
  them out so we can fix it.
- This crate prefers GHC-style note comments, albeit sparingly. See
  `indigo-text`'s `edit` and `grapheme` modules for examples.
