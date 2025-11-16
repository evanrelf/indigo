# Influences

## Text editors

- **[Kakoune]**
  - Good
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
  - Bad
    - C++ codebase I don't want to touch
    - Too much functionality of variable quality implemented in userspace (`rc`)
    - Imperfect grapheme rendering
  - More info in [Kakoune design doc]

[Kakoune]: https://github.com/mawww/kakoune
[Kakoune design doc]: https://github.com/mawww/kakoune/blob/master/doc/design.asciidoc

- **[Vim] and [Neovim]**
  - Good
    - Modal editing
    - Terminal-based user interface
  - Bad
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
  - Good
    - Composition over extension, integrating vs integrated development
      environment, one piece of the Plan 9 IDE
    - File system interface ([9P] server)
    - Challenges the need for syntax highlighting
  - Bad
    - Heavy mouse use
    - No modal editing

[Acme]: http://acme.cat-v.org/
[9P]: https://en.wikipedia.org/wiki/9P_(protocol)

- **[ad]**
  - Good
    - Like Acme but more modern keyboard-based modal editing

[ad]: https://github.com/sminez/ad

- **[CodeMirror]**
  - Good
    - Strong architecture / system design, clean separation of components
    - Persistent/immutable data structures
    - Transactional updates
    - Text editor as a library

[CodeMirror]: https://codemirror.net/

- **[Emacs]**

[Emacs]: https://www.gnu.org/software/emacs/

- **[VS Code]**
  - Good
    - Command palette
    - [Breadcrumbs](https://code.visualstudio.com/docs/editing/editingevolved#_breadcrumbs)
    - [Hot exit](https://code.visualstudio.com/docs/editing/codebasics#_hot-exit)
  - Bad
    - GUI
    - IDE
    - Electron

[VS Code]: https://code.visualstudio.com/

- **[Zed]**
  - Good
    - `SumTree` is so cool
    - CRDTs are cool (overkill for my project though)
    - Outline view
  - Bad
    - GUI
    - IDE
    - Installs language tooling automatically without asking
    - Inconsistent keymap especially regarding window focus, hard to put my
      finger on it but it feels wrong

[Zed]: https://zed.dev/

- **[ed]**
  - Good
    - "Ed is the standard text editor."
    - True path to nirvana
    - Will not corrupt bodily fluids
  - Bad
    - Literally nothing

[ed]: https://www.gnu.org/fun/jokes/ed-msg.en.html

## Blog posts

- "An Engine for an Editor" by matklad
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

- "Readonly Characters Are a Big Deal" by matklad
  https://matklad.github.io/2025/11/10/readonly-characters.html

  "Read only" is a concrete example of the "text with attributes" idea from
  their earlier "An Engine for an Editor" post.

- "Unified Versus Split Diff" by matklad
  https://matklad.github.io/2023/10/23/unified-vs-split-diff.html

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
