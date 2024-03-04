# Indigo

A text editor

## Todo

- **Move buffers above files.**

  The editor's entity is the buffer, not the file. A buffer might be backed by a
  file, but it isn't a file. Indigo's equivalent to Vim's `:bd` command ("buffer
  delete") should not be `:file-delete`, otherwise it sounds like we're deleting
  files from the filesystem.

- **Handle invalid commands gracefully with an error message.**

  Via the `Error` event, and/or via `tracing::error`, and/or via some new
  `error: Option<String>` field in `Editor`?

  Using `tracing` is tempting, but I think I need a way of emitting arbitrary
  events in `indigo-core`, and I'm not sure `tracing` would work... or maybe it
  would?

- **Add in-memory/virtual buffers.**

  The scratch buffer that's created when you start the editor without any
  arguments should be in-memory, not backed by a non-existent file named
  `scratch` (that would be created on save).

  Also, a buffer to store debug output (mostly `tracing` events) would be useful
  so I don't have to explicitly configure logging output.
