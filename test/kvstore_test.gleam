import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/result
import gleeunit
import kvstore
import kvstore/config.{Config}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn insert_get_test() {
  let s = kvstore.new()

  assert Ok(Nil) == s |> kvstore.insert("foo", 5)
  assert Ok(Some(5)) == s |> kvstore.get("foo")

  assert Ok(False) == s |> kvstore.insert_new("foo", 7)
  assert Ok(Some(5)) == s |> kvstore.get("foo")

  assert Ok(True) == s |> kvstore.insert_new("bar", 7)
  assert Ok(Some(7)) == s |> kvstore.get("bar")

  assert Ok(Nil) == s |> kvstore.insert("foo", 1000)
  assert Ok(Some(1000)) == s |> kvstore.get("foo")
}

pub fn insert_delete_test() {
  let s = kvstore.new()

  assert Ok(Nil) == s |> kvstore.insert(0, [1.0, 2.0, 3.0])
  assert Ok(True) == s |> kvstore.contains(0)
  assert Ok(Nil) == s |> kvstore.delete(0)
  assert Ok(None) == s |> kvstore.get(0)
  assert Ok(False) == s |> kvstore.contains(0)
}

pub fn insert_clear_test() {
  let s = kvstore.new()

  assert Ok(Nil) == s |> kvstore.insert("a", 1)
  assert Ok(Nil) == s |> kvstore.insert("b", 2)
  assert Ok(Nil) == s |> kvstore.clear()
  assert Ok(False) == s |> kvstore.contains("a")
  assert Ok(False) == s |> kvstore.contains("b")
}

pub fn drop_test() {
  let s = kvstore.new()

  assert Ok(Nil) == s |> kvstore.insert(0, 1)
  assert Ok(Nil) == s |> kvstore.drop()
  assert s |> kvstore.get(0) |> result.is_error
  assert s |> kvstore.insert(1, 2) |> result.is_error
}

pub fn protected_test() {
  let s = kvstore.new_protected()

  assert Ok(Nil) == s |> kvstore.insert(500, "wow!")

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = s |> kvstore.insert(600, "cool")
    process.send(subj, result)
  })
  assert process.receive_forever(subj) |> result.is_error

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = s |> kvstore.get(500)
    process.send(subj, result)
  })
  assert Ok(Some("wow!")) == process.receive_forever(subj)
}

pub fn private_test() {
  let kv = kvstore.new_private()

  assert Ok(Nil) == kv |> kvstore.insert(True, 5.0)

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = kv |> kvstore.insert(False, -2.3)
    process.send(subj, result)
  })
  assert process.receive_forever(subj) |> result.is_error

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = kv |> kvstore.get(True)
    process.send(subj, result)
  })
  assert process.receive_forever(subj) |> result.is_error
}

pub fn write_config_test() {
  let cfg = config.default()

  // Test that these options successfully construct a table.
  let s0 =
    kvstore.new_with_config(
      Config(..cfg, write_concurrency: config.ParallelWrites),
    )
  assert Ok(Nil) == s0 |> kvstore.insert(0, 1)

  let s1 =
    kvstore.new_with_config(
      Config(..cfg, write_concurrency: config.AutomaticWrites),
    )
  assert Ok(Nil) == s1 |> kvstore.insert(0, 1)
}

pub fn read_config_test() {
  let cfg = config.default()

  // Test that these options successfully construct a table.
  let s0 =
    kvstore.new_with_config(
      Config(..cfg, read_concurrency: config.OptimizedReads),
    )
  assert Ok(Nil) == s0 |> kvstore.insert(0, 1)
}
