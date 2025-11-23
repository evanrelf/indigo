---
name: indigo-cli
description: >
  Supplement unit tests, and compare behavior with Kakoune, by running the full
  editor and executing keys via the CLI.
---

# indigo-cli

## Instructions

### Usage

The `indigo-cli` bin crate is run with the `./bin/cli` shell script. It accepts
the initial input text from `stdin`, keys to execute as the first argument, and
prints the final edited text to `stdout`.

When testing cursor and range behavior, make edits to the text that reveal where
the cursors are located.

### Debug mode

`indigo-cli` has a `--debug` mode where executing `<c-l>` prints keys processed,
char offsets of the primary range, and text changes as a diff, since the last
`<c-l>`. All debug info is printed to `stderr`.

When testing cursor and range behavior, print this debug state often to see the
char offsets.

### Available keys

Key mappings are defined per mode in these files:

- Normal mode: `crates/indigo-core/src/mode/normal.rs`
- Insert mode: `crates/indigo-core/src/mode/insert.rs`
- Goto mode: `crates/indigo-core/src/mode/goto.rs`
- Seek mode: `crates/indigo-core/src/mode/seek.rs`
- Command mode: `crates/indigo-core/src/mode/command.rs` (discouraged with CLI)

### Comparing with Kakoune

Indigo is inspired by Kakoune, so most keys have the same behavior. Try the same
edit in both and compare output to find bugs.

Kakoune's `-f` option starts with the entire buffer selected. To mimic how
Indigo starts with the cursor at the beginning, execute `gk` first.

You must the `-n` option to prevent sourcing configuration. We want to compare
against vanilla, unconfigured Kakoune.

## Examples

Trivial example making no changes:

```
$ echo -ne "hello\nworld\n" | ./bin/cli ''
hello
world
```

Editing the text:

```
$ echo -ne "hello\nworld\n" | ./bin/cli 'fodiHELLO<esc>jgla!'
HELLO
world!
```

Using debug mode:

<pre>
$ echo -ne "hello\nworld\n" | ./bin/cli --debug 'fod<c-l>iHELLO<c-l><esc>jgla!<c-l>'
keys: fod
primary range: anchor=0 head=0
text:
```diff
-hello
+
 world

```
keys: iHELLO
primary range: anchor=5 head=6
text:
```diff
-
+HELLO
 world

```
keys: <esc>jgla!
primary range: anchor=12 head=13
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
$ echo -ne "hello\nworld\n" | kak -n -f 'gkfodiHELLO<esc>jgla!'
HELLO
world!
```
