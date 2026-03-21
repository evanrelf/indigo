# DA1 as a sentinel for queries.

When querying terminal capabilities (e.g. DECRQM to check if a mode is
supported), we avoid timeouts by using Primary Device Attributes (DA1) as a
sentinel. The technique works like this:

1. Send the actual query (e.g. `\x1b[?2048$p` for DECRQM mode 2048)
2. Immediately follow it with a DA1 request (`\x1b[c`)
3. Read events from the terminal in a loop:
  - If we see the expected reply (e.g. DECRPM for mode 2048) -> the terminal
    supports this query, return the reply.
  - If we see a DA1 reply first -> the terminal responded to DA1 without
    answering the query, which means the terminal doesn't recognize the query.
    Return None to indicate "not supported".
  - If we see other events (e.g. resize, keypresses) -> buffer them as pending
    events and keep reading.

This works because terminals process requests in order — if the terminal
understood the query, its reply will arrive before the DA1 reply. If it didn't
understand the query, it silently ignores it and only responds to DA1.
