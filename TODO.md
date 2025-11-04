# Todo

Roughly prioritized list of features I want.

## 1. Saving

- `:w`/`:write` and `:wq`/`:write-quit` commands.

- Write+quit depends on tracking buffer modification state. We'll need a new
  `modified` field on `Buffer` that gets set whenever the text is modified,
  and reset when saving.

- Should not save if no path is set (i.e. a scratch buffer). Don't throw an
  error (editor should not die) but maybe set an error `message`.

- `indigo-core` should not perform I/O, but instead provide an abstract I/O
  interface (i.e. a trait) that is implemented by frontends.
