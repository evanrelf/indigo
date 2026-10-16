- Keep this crate deterministic. For example, `Hash{Map,Set}` must use
  `IndigoBuildHasher`. If you notice existing sources of nondeterminism, call
  them out so we can fix it.
