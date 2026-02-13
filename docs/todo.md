# Todo

- range: Select word for mouse double click
- range: Select line for mouse triple click

## Friction in use

- No `/` search
- No `o` or `O`
- No auto-scroll to keep primary range in view
- No `C` or `<a-C>`

## Keys

Most will have the same behavior as Kakoune, but not all (e.g. `q` is the
backward word key, not `b`).

See also: https://github.com/mawww/kakoune/blob/master/doc/pages/keys.asciidoc.

### Insert mode

- [x] Characters
- [x] `<bs>` (delete before cursor)
- [x] `<del>` (delete after cursor)
- [x] `<ret>` (insert newline)
- [x] `<tab>` (insert tab)
- [x] `<c-u>` (scroll half page up)
- [x] `<c-d>` (scroll half page down)
- [x] `<c-b>` (scroll page up)
- [x] `<c-f>` (scroll page down)
- [x] `<esc>` (leave insert mode)
- [ ] `<c-r>` (insert register contents)
- [ ] `<c-v>` (insert keystroke literally)
- [ ] `<a-;>` (escape to normal for one command)

### Normal mode

- [x] `h` (select left)
- [x] `j` (select down)
- [x] `k` (select up)
- [x] `l` (select right)
- [x] `H` (extend left)
- [x] `J` (extend down)
- [x] `K` (extend up)
- [x] `L` (extend right)
- [ ] `q` (select word left)
- [ ] `Q` (extend word left)
- [ ] `<a-q>` (select WORD left)
- [ ] `<a-Q>` (extend WORD left)
- [ ] `w` (select word right)
- [ ] `W` (extend word right)
- [ ] `<a-w>` (select WORD right)
- [ ] `<a-W>` (extend WORD right)
- [ ] `e` (select to word end)
- [ ] `E` (extend to word end)
- [ ] `<a-e>` (select to WORD end)
- [ ] `<a-E>` (extend to WORD end)
- [x] `f` (find next char)
- [x] `F` (find next char, extend)
- [x] `<a-f>` (find prev char)
- [x] `<a-F>` (find prev char, extend)
- [x] `t` (till next char)
- [x] `T` (till next char, extend)
- [x] `<a-t>` (till prev char)
- [x] `<a-T>` (till prev char, extend)
- [ ] `<a-.>` (repeat last object/f/t selection)
- [ ] `m` (select to matching bracket)
- [ ] `M` (extend to matching bracket)
- [ ] `<a-m>` (select to prev matching bracket)
- [ ] `<a-M>` (extend to prev matching bracket)
- [x] `i` (insert before cursor)
- [x] `I` (insert at non-blank line start)
- [x] `a` (insert after cursor)
- [x] `A` (insert at line end)
- [x] `d` (delete selection)
- [ ] `<a-d>` (delete without yanking)
- [x] `c` (delete and insert)
- [ ] `<a-c>` (delete and insert without yanking)
- [ ] `y` (yank selection)
- [ ] `p` (paste after)
- [ ] `P` (paste before)
- [ ] `<a-p>` (paste all after)
- [ ] `<a-P>` (paste all before)
- [ ] `r` (replace each char)
- [ ] `R` (replace with yanked)
- [ ] `<a-R>` (replace with every yanked)
- [ ] `o` (open line below)
- [ ] `O` (open line above)
- [ ] `<a-o>` (add empty line below)
- [ ] `<a-O>` (add empty line above)
- [ ] `<a-j>` (join lines)
- [ ] `<a-J>` (join lines, select spaces)
- [ ] `<a-_>` (merge contiguous selections)
- [ ] `.` (repeat last insert)
- [ ] `>` (indent)
- [ ] `<` (unindent)
- [ ] `<a->>` (indent including empty lines)
- [ ] `<a-<>` (unindent, keep incomplete indent)
- [x] `u` (undo)
- [x] `U` (redo)
- [ ] `<a-u>` (undo selection change)
- [ ] `<a-U>` (redo selection change)
- [ ] `<c-j>` (move forward in change history)
- [ ] `<c-k>` (move backward in change history)
- [ ] `&` (align selections)
- [ ] `<a-&>` (copy indent from main selection)
- [ ] `` ` `` (to lowercase)
- [ ] `~` (to uppercase)
- [ ] `` <a-`> `` (swap case)
- [ ] `@` (tabs to spaces)
- [ ] `<a-@>` (spaces to tabs)
- [ ] `_` (unselect surrounding whitespace)
- [ ] `<a-)>` (rotate selection contents)
- [ ] `<a-(>` (rotate selection contents backward)
- [ ] `<+>` (duplicate each selection)
- [ ] `<a-+>` (merge overlapping selections)
- [ ] `x` (select full lines)
- [ ] `X` (extend to full lines)
- [ ] `<a-x>` (trim to full lines)
- [x] `%` (select whole buffer)
- [ ] `<a-h>` (select to line begin)
- [ ] `<a-l>` (select to line end)
- [x] `<c-u>` (scroll half page up)
- [x] `<c-d>` (scroll half page down)
- [x] `<c-b>` (scroll page up)
- [x] `<c-f>` (scroll page down)
- [x] `g` (goto mode)
- [x] `;` (reduce to cursor)
- [x] `,` (keep main selection only)
- [ ] `<a-,>` (clear main selection)
- [x] `<a-;>` (flip selection direction)
- [x] `<a-:>` (ensure selections face forward)
- [x] `:` (command prompt)
- [x] `<esc>` (cancel / reset)
- [ ] `<space>` (user mode)
- [ ] Ctrl + left click (add cursor)

### Goto mode

- [x] `j` (go to buffer bottom)
- [x] `J` (extend to buffer bottom)
- [x] `k` (go to buffer top)
- [x] `K` (extend to buffer top)
- [x] `h` (go to line start)
- [x] `H` (extend to line start)
- [x] `l` (go to line end)
- [x] `L` (extend to line end)
- [x] `i` (go to first non-blank)
- [x] `I` (extend to first non-blank)
- [ ] `g` (go to buffer top)
- [ ] `e` (go to last char of buffer)
- [ ] `t` (go to first displayed line)
- [ ] `c` (go to middle displayed line)
- [ ] `b` (go to last displayed line)
- [ ] `a` (go to alternate buffer)
- [ ] `f` (open file under selection)
- [ ] `.` (go to last modification)

### View mode

- [ ] `v` (view mode)
- [ ] `V` (lock view mode)

### Searching

- [ ] `/` (search forward)
- [ ] `<a-/>` (search backward)
- [ ] `?` (extend to next match)
- [ ] `<a-?>` (extend to prev match)
- [ ] `n` (select next match)
- [ ] `N` (add next match to selections)
- [ ] `<a-n>` (select prev match)
- [ ] `<a-N>` (add prev match to selections)
- [ ] `*` (set search pattern from selection)
- [ ] `<a-*>` (set search pattern, verbatim)

### Multiple selections

- [ ] `s` (select regex matches within)
- [ ] `S` (split on regex)
- [ ] `<a-s>` (split on line boundaries)
- [ ] `<a-S>` (select first and last chars)
- [ ] `C` (duplicate selections down)
- [ ] `<a-C>` (duplicate selections up)
- [ ] `<a-k>` (keep matching selections)
- [ ] `<a-K>` (clear matching selections)
- [ ] `)` (rotate main selection forward)
- [ ] `(` (rotate main selection backward)

### Object selection

- [ ] `<a-a>` (select whole object)
- [ ] `<a-i>` (select inner object)
- [ ] `[` (select to object start)
- [ ] `]` (select to object end)
- [ ] `{` (extend to object start)
- [ ] `}` (extend to object end)
- [ ] `<a-[>` (select to inner object start)
- [ ] `<a-]>` (select to inner object end)
- [ ] `<a-{>` (extend to inner object start)
- [ ] `<a-}>` (extend to inner object end)

### Marks

- [ ] `Z` (save selections to register)
- [ ] `z` (restore selections from register)
- [ ] `<a-z>` (combine register with current)
- [ ] `<a-Z>` (combine current with register)

### Macros

- [ ] Record macro
- [ ] Play macro

### Jump list

- [ ] `<c-i>` (jump forward)
- [ ] `<c-o>` (jump backward)
- [ ] `<c-s>` (save selection to jump list)

### Changes through external programs

- [ ] `|` (pipe through filter, replace)
- [ ] `<a-|>` (pipe through filter, ignore output)
- [ ] `$` (pipe and keep if exit 0)
- [ ] `!` (insert command output before)
- [ ] `<a-!>` (append command output after)
