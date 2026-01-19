import gleam/option.{type Option}

pub type KVStore(key, value)

pub fn new() -> KVStore(key, value) {
  create_table(Public)
}

pub fn new_protected() -> KVStore(key, value) {
  create_table(Protected)
}

pub fn new_private() -> KVStore(key, value) {
  create_table(Private)
}

@external(erlang, "kvstore_ffi", "kvstore_get")
pub fn get(table: KVStore(k, v), key: k) -> Result(Option(v), Nil)

@external(erlang, "kvstore_ffi", "kvstore_contains")
pub fn contains(table: KVStore(k, v), key: k) -> Result(Bool, Nil)

@external(erlang, "kvstore_ffi", "kvstore_insert")
pub fn insert(table: KVStore(k, v), key: k, value: v) -> Result(Nil, Nil)

@external(erlang, "kvstore_ffi", "kvstore_insert_new")
pub fn insert_new(table: KVStore(k, v), key: k, value: v) -> Result(Bool, Nil)

@external(erlang, "kvstore_ffi", "kvstore_delete")
pub fn delete(table: KVStore(k, v), key: k) -> Result(Nil, Nil)

@external(erlang, "kvstore_ffi", "kvstore_drop")
pub fn drop(table: KVStore(k, v)) -> Result(Nil, Nil)

// -------
// Helpers
// -------

type Access {
  // Read and write access for other processes.
  Public

  // Read-only access for other processes.
  Protected

  // No access for other processes.
  Private
}

@external(erlang, "kvstore_ffi", "kvstore_new")
fn create_table(access: Access) -> KVStore(key, value)
