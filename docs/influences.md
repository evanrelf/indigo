# Influences

Disclaimer: this is not a thorough review of every influence, nor have I written
down everything I think about them.

## Writing

- **"An Engine for an Editor" by matklad**

  https://matklad.github.io/2023/03/08/an-engine-for-an-editor.html

  > The foundation is a text with attributes — a pair of a string and a map
  > from string’s subranges to key-value dictionaries. Attributes express
  > presentation (color, font, text decoration), but also semantics. A range
  > of text can be designated as clickable. Or it can specify a custom keymap,
  > which is only active when the cursor is on this range.

  I'm not going to build the general purpose text-based computing substrate
  imagined in this post. But I think this "text with attributes" idea is a
  really good one for my editor. Syntax highlighting, goto definition, red
  squiggles, etc could all be implemented with this.

- **"Readonly Characters Are a Big Deal" by matklad**

  https://matklad.github.io/2025/11/10/readonly-characters.html

  "Read only" is a concrete example of the "text with attributes" idea from
  their earlier "An Engine for an Editor" post.

- **"Unified Versus Split Diff" by matklad**

  https://matklad.github.io/2023/10/23/unified-vs-split-diff.html

  Why "diff reviews" suck and quality code review requires contextualizing
  changes in the full codebase that you can edit and explore.

  > For a large change, I don’t want to do a “diff review”, I want to
  > do a proper code review of a codebase at a particular instant in time,
  > paying specific attention to the recently changed areas, but mostly just
  > doing general review, as if I am writing the code. I need to run tests,
  > use goto definition and other editor navigation features, apply local
  > changes to check if some things could have been written differently, look
  > at the wider context to notice things that should have been changed, and
  > in general notice anything that might be not quite right with the codebase,
  > irrespective of the historical path to the current state of the code.

  I'd love to support this kind of code review workflow in my text editor.
  Showing the green added text _and_ red deleted text, like reviewing a PR on
  GitHub, but with real files locally. Then I can edit them to make the
  formatting easier to read, try out an idea before suggesting it, jump around
  with goto definition, see the full context in which the change is being made,
  etc.

- **Addressing Editor Content by Marijn Haverbeke**

  https://marijnhaverbeke.nl/blog/addressing-editor-content.html

  Talks about why offsets are the right answer for most projects.

  > Every text editor system, whether it works with plain or rich text,
  > needs some way to refer to specific positions in the document. The very
  > first reason one runs into this, when implementing such a system, is for
  > representing the cursor and selection. But in a mature editor, you'll be
  > doing a lot more things that refer to document positions—representing
  > changes, displaying user interface elements in the text, or tracking
  > metadata about a piece of content.

  I personally think byte offsets are best. Not char offsets (less efficient
  than bytes), not grapheme offsets (they're variable width, and the count
  changes as you append text, so it's not a monoidal metric you can track in a
  rope), not any kind of index either (offsets are better for bar cursors which
  can be represented as zero-width selections, inclusive ranges of bytes, having
  some representation of a cursor at/past EOF like in an empty file, etc).

- **Facets as Composable Extension Points by Marijn Haverbeke**

  https://marijnhaverbeke.nl/blog/facets.html

  The extension system designed for CodeMirror 6.

  > A facet, in this system, defines an extension point. It takes any number
  > of input values and produces an output value. Examples of facets are...
  >
  > - Event handlers, where individual extension can define function that handle
  >   a given event.
  >
  > - Editor configuration, like the tab size and whether content is read-only.
  >
  > - The set of markers to style the content with (for example syntax
  >   highlighting).
  >
  > - The set of gutters to show next to the content.

## Text editors

- **[Kakoune]**
  - I like
    - Elegant, well designed
    - Few orthogonal features that are powerful when composed
    - Noun before verb editing language
    - Interactive visual feedback
    - Multiple cursors
    - Minimize command mode for editing, maximize normal mode editing language
      (compared to Vim/Neovim)
    - Composition over extension, integrating vs integrated development
      environment, one piece of the Unix IDE
    - Self documenting
    - No dependencies except for modern C++ compiler and standard library
  - I don't like
    - C++ codebase I don't want to touch
    - Too much functionality of variable quality implemented in userspace (`rc`)
    - Imperfect grapheme rendering
  - More info in [Kakoune design doc]

[Kakoune]: https://github.com/mawww/kakoune
[Kakoune design doc]: https://github.com/mawww/kakoune/blob/master/doc/design.asciidoc

- **[Vim] and [Neovim]**
  - I like
    - Modal editing
    - Terminal-based user interface
  - I don't like
    - Has accreted so much functionality over the decades
    - No multiple cursors
    - Verb before noun, and heavy reliance on command mode (e.g. `s` and `g`)
      and macros for editing tasks, leads to less interactive editing with poor
      feedback that allows for more mistakes that are less forgiving to fix
    - Bad non-standard regex dialect, nobody should have to think about "magic"
    - Design is not clean and orthogonal like Kakoune
    - Neovim customization scene is as fast paced and immature (experience level
      of vocal users, quality of popular software) as frontend dev
    - Imperfect grapheme rendering

[Vim]: https://www.vim.org/
[Neovim]: https://neovim.io/

- **[Acme]**
  - I like
    - Composition over extension, integrating vs integrated development
      environment, one piece of the Plan 9 IDE
    - File system interface ([9P] server)
    - Challenges the need for syntax highlighting
  - I don't like
    - Heavy mouse use
    - No modal editing

[Acme]: http://acme.cat-v.org/
[9P]: https://en.wikipedia.org/wiki/9P_(protocol)

- **[ad]**
  - I like
    - Like Acme but more modern keyboard-based modal editing

[ad]: https://github.com/sminez/ad

- **[CodeMirror]**
  - I like
    - Strong architecture / system design, clean separation of components
    - Persistent/immutable data structures
    - Transactional updates
    - Text editor as a library

[CodeMirror]: https://codemirror.net/

- **[Emacs]**
  - I like
    - Text with attributes
  - I don't like
    - Slow, more or less single threaded

[Emacs]: https://www.gnu.org/software/emacs/

- **[VS Code]**
  - I like
    - Command palette
    - [Breadcrumbs](https://code.visualstudio.com/docs/editing/editingevolved#_breadcrumbs)
    - [Hot exit](https://code.visualstudio.com/docs/editing/codebasics#_hot-exit)
  - I don't like
    - GUI
    - IDE
    - Electron

[VS Code]: https://code.visualstudio.com/

- **[Zed]**
  - I like
    - [`SumTree`] is so cool
    - CRDTs are cool (overkill for my project though)
    - Outline view
  - I don't like
    - GUI
    - IDE
    - Installs language tooling automatically without asking
    - Inconsistent keymap especially regarding window focus, hard to put my
      finger on it but it feels wrong

[Zed]: https://zed.dev/
[`SumTree`]: https://github.com/zed-industries/zed/tree/main/crates/sum_tree

- **[ed]**
  - I like
    - "Ed is the standard text editor."
    - True path to nirvana
    - Will not corrupt bodily fluids
  - I don't like
    - Literally nothing

[ed]: https://www.gnu.org/fun/jokes/ed-msg.en.html
