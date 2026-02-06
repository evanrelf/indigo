---
name: indigo-cli
description: >
  Supplement unit tests, and compare behavior with Kakoune, by running the full
  editor and executing keys via the CLI.
---

# indigo-cli

## Usage

The `indigo-cli` bin crate is run with the `./bin/cli` shell script. It accepts
the initial buffer text from `stdin`, keys to execute as the first argument, and
prints the final edited text to `stdout`.

When testing the behavior of the selection / ranges / cursors, make edits to the
text that reveal where the cursors are located. For example, if you want to test
whether moving to the beginning of the line (`gh`) works, follow that action by
inserting some unique text, then check where that text is in the output to
intuit the final cursor position.

## Debug mode

`indigo-cli` has a `--debug` mode with more verbose output on `stderr`.

- Prints when keys are unhandled
- When executing `<c-l>`, prints:
  - Keys processed
  - Char offsets of the primary range
  - Text changes as a diff

When testing cursor and range behavior, print this debug state often to see the
char offsets.

## Available keys

Key mappings are defined per mode in these files:

- Normal mode: `crates/indigo-core/src/mode/normal.rs`
- Insert mode: `crates/indigo-core/src/mode/insert.rs`
- Goto mode: `crates/indigo-core/src/mode/goto.rs`
- Seek mode: `crates/indigo-core/src/mode/seek.rs`
- Prompt mode: `crates/indigo-core/src/mode/prompt.rs`
- Command mode: `crates/indigo-core/src/mode/command.rs` (discouraged with CLI)

## Comparing with Kakoune

Indigo is inspired by Kakoune, so most keys have the same behavior. Try the same
edit in both and compare output to find bugs.

Kakoune's `-f` option starts with the entire buffer selected, but Indigo starts
with a cursor at the beginning. To compensate for this, execute `gk` first in
Kakoune.

You must use the `-n` option to prevent sourcing configuration. We want to
compare against vanilla, unconfigured Kakoune.

## Examples

Trivial example making no changes:

```
$ echo -ne "hello\nworld\n" | ./bin/cli ''
hello
world
```

Editing the text:

```
$ echo -ne "hello\nworld\n" | ./bin/cli 'focHELLO<esc>jgla!'
HELLO
world!
```

Whitespace and comments are ignored, so you can write "literate" keys:

```
$ echo -ne "hello\nworld\n" | ./bin/cli '
      # Extend the selection to the first `o` character
      fo

      # Delete the selection and enter insert mode
      c

      # We are in insert mode, so `HELLO` enters the text literally
      HELLO

      # Back to normal mode
      <esc>

      # Move down to second line
      j

      # Move to end of line
      gl

      # Enter insert mode after currently selected character (`d`)
      a

      # Add an exclamation point to the end of the word `world`
      !
  '
HELLO
world!
```

Using debug mode:

<pre>
$ echo -ne "hello\nworld\n" | ./bin/cli --debug 'foc<c-l>HELLO<c-l><esc>jgla!<c-l>'
keys: foc
primary range: tail=0 head=1
text:
```diff
-hello
+
 world

```
keys: HELLO
primary range: tail=5 head=6
text:
```diff
-
+HELLO
 world

```
keys: <esc>jgla!
primary range: tail=12 head=13
text:
```diff
 HELLO
-world
+world!

```
HELLO
world!
</pre>

Performing the same edit in Kakoune:

```
$ echo -ne "hello\nworld\n" | kak -n -f 'gkfocHELLO<esc>jgla!'
HELLO
world!
```
