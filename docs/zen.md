# The Zen of Indigo

- Composable is better than extensible

## Workshopping

- Simplicity, few orthogonal features, kill your darlings
  - Nothing gimmicky or of questionable utility. Maintain a high bar of quality
    and multi-purpose utility for features.
  - Regex select + multiple cursors is better than macros + command-based
    search and replace. Get back to the normal mode editing language as soon as
    possible. Do not introduce a whole separate command language.

- Earn the user's trust, and never betray it
  - Accept that mistakes are a fact of life, and build with that in mind. Editor
    failure should be graceful (auto save, detailed and helpful error messages,
    etc.). User error should be low stakes (undo should always be available).
    Build user confidence by removing their fear (like Rust's fearless
    concurrency).

- Optimize for maintaining flow and being intuitive, rather than being fast or
  efficient
  - `gk`+`gj` is better than `gg`+`G`. `u`+`U` is better than `u`+`<C-r>`.

- Shut up
  - Don't print when hitting the top or bottom of a file (Emacs), the number of
    bytes saved to disk (Vim), etc.
  - Provide information contextually. Progressive disclosure when needed.

- The user is an adult. Using this tool is an investment which should be
  respected and rewarded.
  - Use the screen space you're given well.
  - Don't water things down. No Gmail/GCal "comfy" mode. No hamburger/3dot
    menus.
