# kvstore

Typed key-value stores backed by ETS tables.

[![Package Version](https://img.shields.io/hexpm/v/kvstore)](https://hex.pm/packages/kvstore)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/kvstore/)

```sh
gleam add kvstore@1
```

To create a key-value store with default configuration, use `kvstore.new`:

```gleam
import kvstore

pub fn main() -> Nil {
  let store = kvstore.new()
  let _ = kvstore.insert(store, "foo", 5)
  let _ = kvstore.insert(store, "bar", 6)
  assert kvstore.get(store, "foo") == Ok(Some(5))
  assert kvstore.get(store, "bar") == Ok(Some(6))
}
```

By default, stores can be read from and written to from other processes:

```gleam
import kvstore
import gleam/erlang/process

pub fn main() -> Nil {
  let store = kvstore.new()
  let _ = kvstore.insert(store, 0, "foo")

  let subj = process.new_subject()
  process.spawn(fn() {
    // Write from another process
    let _ = kvstore.insert(store, 1, "bar")
    process.send(subj, Nil)
  })
  
  // Wait until after the write is confirmed, then see its value in the store
  let assert Ok(Nil) = process.receive(subj, 100)
  assert kvstore.get(store, 1) == Ok(Some("bar"))
}
```

The access controls for non-owner processes are configurable, along with
performance tuning for concurrent reads or writes. 

For more information, see the documentation at <https://hexdocs.pm/kvstore>.

