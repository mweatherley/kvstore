import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/result
import gleeunit
import kvstore

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn insert_get_test() {
  let kv = kvstore.new()

  assert Ok(Nil)
    == kv
    |> kvstore.insert("foo", 5)
  assert Ok(Some(5)) == kv |> kvstore.get("foo")

  let _ = kv |> kvstore.insert_new("foo", 7)
  assert Ok(Some(5)) == kv |> kvstore.get("foo")

  let _ = kv |> kvstore.insert("foo", 1000)
  assert Ok(Some(1000)) == kv |> kvstore.get("foo")

  let _ = kv |> kvstore.delete("foo")
  assert Ok(None) == kv |> kvstore.get("foo")

  let _ = kv |> kvstore.drop
  assert Error(Nil) == kv |> kvstore.get("bar")
}

pub fn protected_test() {
  let kv = kvstore.new_protected()

  assert Ok(Nil) == kv |> kvstore.insert(500, "wow!")

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = kv |> kvstore.insert(600, "cool")
    process.send(subj, result)
  })
  assert process.receive_forever(subj) |> result.is_error

  let subj = process.new_subject()
  process.spawn(fn() {
    let result = kv |> kvstore.get(500)
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
