# indigo-kernel

A transactional substrate for concurrent editing. Agents (human, plugin, code
formatter, etc) edit snapshots independently, and the kernel reconciles their
changes, converging on a single canonical state.
