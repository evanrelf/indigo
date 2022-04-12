## `indigo_state` Notes

- Add middleware? See [this](https://docs.rs/redux-rs/latest/redux_rs/trait.MiddleWare.html)
  for an example.
- Allow reducers to request multiple pieces of state?
  - Separate `StoreQuery` trait for handling `Query<(Foo, Bar, Baz)>`?
  - Allow requesting context? (e.g. read-only access to another field)
- Allow reducers to handle multiple different events?
- Panic when action isn't handled by any reducers?
- Priorities for reducers?
  - Levels
    - `Override` (don't let anything else handle this action)
    - `Before{High,Medium,Low}` (handle action before the default)
    - `Default`
    - `After{High,Medium,Low}` (handle action after the default)
    - `Fallback` (only run if nothing else handled this action)
  - Panic when priority is ambiguous? (e.g. multiple `Override` or
    `Fallback`-level reducers)
- Add more helpful panic message(s)? (e.g. using `std::any::type_name`)
- Allow listener functions to subscribe to state changes?
  - Should they subscribe to specific pieces of state?
- Allow removing reducers?
- Allow removing state?
  - Could cause panic due to lingering reducer depending on that state, so
    probably not a good idea. What does `bevy_ecs` do?
- Can/should implement `Send` and `Sync` for `Store`?
  - Probably forget about it until the need comes up in practice
  - Maybe simply `indigo_state::Store`: `type Store = StoreImpl<Any>`,
    `indigo_state::sync::Store`: `type Store = StoreImpl<Any + Send + Sync>`?
  - Might require rewriting with things from `std::sync`
- Multiple implementations (e.g. `Store` and `sync::Store`) could implement a
  `Store` trait with methods like `new`, `dispatch`, etc.
- `redux-rs` has a cool `Selector` trait that allows querying for arbitrary
  things in the store (not just one or many fields, but could combine fields or
  derive values, etc.)