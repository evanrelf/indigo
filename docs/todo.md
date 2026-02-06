# Todo

- cursor: Adopt anchors
- range: Adopt anchors
- selection: Adopt anchors
- buffer: Track selection history with anchors
- text: Make `apply` method private, move everyone to `insert`+`delete`+anchors
- range: Correct cursor positioning
- fs: Add support for path canonicalization
- buffer: Canonicalize path in `open`
- buffer: Store history of ranges
- editor: Hold multiple buffers
- range: Select word (for mouse double click)
- range: Select line (for mouse triple click)

## Friction in use

- No `/` search
- No `123g` to go to line
- No `o` or `O`
- No auto-scroll to keep primary range in view

## Keys

Most will have the same behavior as Kakoune, but not all (e.g. `q` is the
backward word key, not `b`).

### Insert mode

- [x] Characters
- [x] `<bs>`
- [x] `<del>`
- [x] `<ret>`
- [x] `<tab>`
- [x] `<c-u>`
- [x] `<c-d>`
- [x] `<c-b>`
- [x] `<c-f>`
- [x] `<esc>`

### Normal mode

- [x] `h`
- [x] `j`
- [x] `k`
- [x] `l`
- [x] `H`
- [x] `J`
- [x] `K`
- [x] `L`
- [ ] `q`
- [ ] `Q`
- [ ] `<a-q>`
- [ ] `<a-Q>`
- [ ] `w`
- [ ] `W`
- [ ] `<a-w>`
- [ ] `<a-W>`
- [ ] `e`
- [ ] `E`
- [ ] `<a-e>`
- [ ] `<a-E>`
- [x] `f`
- [x] `F`
- [x] `<a-f>`
- [x] `<a-F>`
- [x] `t`
- [x] `T`
- [x] `<a-t>`
- [x] `<a-T>`
- [x] `i`
- [x] `I`
- [x] `a`
- [x] `A`
- [x] `d`
- [ ] `<a-d>`
- [x] `c`
- [ ] `<a-c>`
- [ ] `y`
- [ ] `p`
- [ ] `P`
- [ ] `<a-p>`
- [ ] `<a-P>`
- [ ] `r`
- [ ] `R`
- [ ] `<a-R>`
- [ ] `o`
- [ ] `O`
- [ ] `<a-o>`
- [ ] `<a-O>`
- [ ] `<a-j>`
- [ ] `.`
- [ ] `m`
- [ ] `>`
- [ ] `<`
- [x] `u`
- [x] `U`
- [ ] `<a-u>`
- [ ] `<a-U>`
- [ ] `x`
- [ ] `X`
- [x] `%`
- [x] `<c-u>`
- [x] `<c-d>`
- [x] `<c-b>`
- [x] `<c-f>`
- [x] `g`
- [x] `;`
- [x] `,`
- [x] `<a-;>`
- [x] `<a-:>`
- [x] `:`
- [x] `<esc>`
- [ ] Ctrl + left click

### Goto Mode

- [x] `j`
- [x] `J`
- [x] `k`
- [x] `K`
- [x] `h`
- [x] `H`
- [x] `l`
- [x] `L`
- [x] `i`
- [x] `I`
