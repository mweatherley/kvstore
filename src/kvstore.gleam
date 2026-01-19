import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import internal/convert
import kvstore/config.{type Config, Config, Private, Protected}

/// A table storing at most one value for each key, backed by an ETS "set" table.
///
/// Concretely, this value is the identifier for such a table; it may remain in
/// scope even if the table is dropped.
///
/// For more information on ETS tables, see the [Erlang documentation].
///
/// [Erlang documentation]: https://www.erlang.org/doc/apps/stdlib/ets.html
///
pub type KVStore(key, value)

/// Create a new key-value store.
///
/// The table is owned by the calling process; it will be dropped when the owner
/// process terminates.
///
/// For the sake of simplicity, tables created by this function use [public
/// access controls] and default performance options. For other access options,
/// see [`new_protected`] and [`new_private`].
///
/// For performance tuning options, see [`new_with_config`].
///
/// [public access controls]: kvstore/config.html#Access
/// [`new_protected`]: kvstore.html#new_protected
/// [`new_private`]: kvstore.html#new_private
/// [`new_with_config`]: kvstore.html#new_with_config
/// 
pub fn new() -> KVStore(key, value) {
  new_with_config(config.default())
}

/// Create a new key-value store with [protected access controls] and default
/// performance options.
///
/// For performance tuning options, see [`new_with_config`].
/// 
/// [protected access controls]: kvstore/config.html#Access
/// [`new_with_config`]: kvstore.html#new_with_config
/// 
pub fn new_protected() -> KVStore(key, value) {
  new_with_config(Config(..config.default(), access: Protected))
}

/// Create a new key-value store with [private access controls] and default
/// performance options.
///
/// For performance tuning options, see [`new_with_config`].
/// 
/// [private access controls]: kvstore/config.html#Access
/// [`new_with_config`]: kvstore.html#new_with_config
/// 
pub fn new_private() -> KVStore(key, value) {
  new_with_config(Config(..config.default(), access: Private))
}

/// Create a new key-value store with the given configuration.
/// 
pub fn new_with_config(config: Config) {
  create_table(convert.into_options(config))
}

/// Get the value for a given key in the store.
///
/// This function will return an error if the table is inaccessible to read by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_get")
pub fn get(table: KVStore(k, v), key: k) -> Result(Option(v), Nil)

/// Returns true if a value for the given key exists in the store.
///
/// This function will return an error if the table is inaccessible to read by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_contains")
pub fn contains(table: KVStore(k, v), key: k) -> Result(Bool, Nil)

/// Insert a given key-value pair into the store, overwriting any existing entry.
///
/// This function will return an error if the table is inaccessible to write by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_insert")
pub fn insert(table: KVStore(k, v), key: k, value: v) -> Result(Nil, Nil)

/// Insert a given key-value pair into the store if an entry does not already exist.
/// Returns a boolean indicating whether a new value was inserted.
///
/// This function will return an error if the table is inaccessible to write by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_insert_new")
pub fn insert_new(table: KVStore(k, v), key: k, value: v) -> Result(Bool, Nil)

/// Delete any existing entry for the given key from the store.
///
/// This function will return an error if the table is inaccessible to write by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_delete")
pub fn delete(table: KVStore(k, v), key: k) -> Result(Nil, Nil)

/// Clear all entries from the table. This action is performed atomically.
///
/// This function will return an error if the table is inaccessible to write by
/// the calling process, either because of access controls or because the table
/// has been dropped.
/// 
@external(erlang, "kvstore_ffi", "kvstore_clear")
pub fn clear(table: KVStore(k, v)) -> Result(Nil, Nil)

/// Drop the entire table.
///
/// To clear all entries instead, use [`clear`].
///
/// This function will return an error if the table is inaccessible to write by
/// the calling process, either because of access controls or because the table
/// has been dropped.
///
/// [`clear`]: kvstore.html#clear
/// 
@external(erlang, "kvstore_ffi", "kvstore_drop")
pub fn drop(table: KVStore(k, v)) -> Result(Nil, Nil)

// -------
// Helpers
// -------

@external(erlang, "kvstore_ffi", "kvstore_new")
fn create_table(options: List(Dynamic)) -> KVStore(key, value)
