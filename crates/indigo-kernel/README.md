# indigo-kernel

A transactional substrate for concurrent editing. Agents (human, plugin, code
formatter, etc) edit snapshots independently, and the kernel reconciles their
changes, converging on a single canonical state.

## References

- ["Concurrent Programming with Revisions and Isolation Types"](https://dl.acm.org/doi/epdf/10.1145/1932682.1869515)
- ["The Art of the Fugue: Minimizing Interleaving in Collaborative Text Editing"](https://arxiv.org/abs/2305.00583)
