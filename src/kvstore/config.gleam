import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom

pub type Config {
  Config(
    access: Access,
    write_concurrency: WriteConcurrency,
    read_concurrency: ReadConcurrency,
  )
}

pub fn default() -> Config {
  Config(
    access: Public,
    write_concurrency: SerializedWrites,
    read_concurrency: Default,
  )
}

pub type WriteConcurrency {
  SimultaneousWrites
  SerializedWrites
  Automatic
}

fn write_concurrency_option(conc: WriteConcurrency) -> Dynamic {
  let conc_setting = case conc {
    SimultaneousWrites -> dynamic.bool(True)
    SerializedWrites -> dynamic.bool(False)
    Automatic -> dynamic_atom("auto")
  }

  dynamic.array([
    dynamic_atom("write_concurrency"),
    conc_setting,
  ])
}

pub type ReadConcurrency {
  Optimized
  Default
}

fn read_concurrency_option(conc: ReadConcurrency) -> Dynamic {
  let conc_setting = case conc {
    Optimized -> dynamic.bool(True)
    Default -> dynamic.bool(False)
  }

  dynamic.array([dynamic_atom("read_concurrency"), conc_setting])
}

pub type Access {
  /// Read and write access for other processes.
  /// 
  Public

  /// Read-only access for other processes.
  /// 
  Protected

  /// No access for other processes.
  /// 
  Private
}

fn access_option(access: Access) -> Dynamic {
  let atom = case access {
    Public -> atom.create("public")
    Protected -> atom.create("protected")
    Private -> atom.create("private")
  }
  atom.to_dynamic(atom)
}

pub fn into_options(config: Config) -> List(Dynamic) {
  [
    dynamic_atom("set"),
    access_option(config.access),
    write_concurrency_option(config.write_concurrency),
    read_concurrency_option(config.read_concurrency),
  ]
}

// Helpers

fn dynamic_atom(a: String) -> Dynamic {
  atom.create(a) |> atom.to_dynamic
}
