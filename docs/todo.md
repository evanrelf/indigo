# Todo

- key: Fix parser issues
- key: Add context to parsers
- range: Correct cursor positioning
- selection: Finish integrating
- selection: Hold multiple ranges
- window: Define invariants
- window: Add correction logic
- window: Scroll to selection
- fs: Add support for path canonicalization
- buffer: Canonicalize path in `open`
- buffer: Store history of ranges
- editor: Hold multiple buffers
- ot: Implement `EditSeq::compose`

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
- [ ] `I`
- [x] `a`
- [ ] `A`
- [x] `d`
- [ ] `<a-d>`
- [ ] `c`
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
- [ ] `u`
- [ ] `U`
- [ ] `<a-u>`
- [ ] `<a-U>`
- [ ] `x`
- [ ] `X`
- [ ] `%`
- [x] `<c-u>`
- [x] `<c-d>`
- [x] `<c-b>`
- [x] `<c-f>`
- [x] `g`
- [x] `;`
- [x] `<a-;>`
- [x] `<a-:>`
- [x] `:`
- [x] `<esc>`

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
