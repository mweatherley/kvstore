import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import kvstore/config.{type Config, Config, Private, Protected}

/// A type-safe key-value store. 
pub type KVStore(key, value)

pub fn new() -> KVStore(key, value) {
  new_with_config(config.default())
}

pub fn new_protected() -> KVStore(key, value) {
  new_with_config(Config(..config.default(), access: Protected))
}

pub fn new_private() -> KVStore(key, value) {
  new_with_config(Config(..config.default(), access: Private))
}

pub fn new_with_config(config: Config) {
  create_table(config.into_options(config))
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

@external(erlang, "kvstore_ffi", "kvstore_clear")
pub fn clear(table: KVStore(k, v)) -> Result(Nil, Nil)

@external(erlang, "kvstore_ffi", "kvstore_drop")
pub fn drop(table: KVStore(k, v)) -> Result(Nil, Nil)

// -------
// Helpers
// -------

@external(erlang, "kvstore_ffi", "kvstore_new")
fn create_table(options: List(Dynamic)) -> KVStore(key, value)
