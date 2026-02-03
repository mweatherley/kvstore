import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import kvstore/config

/// Create the write concurrency option for ETS configuration.
fn write_concurrency_option(conc: config.WriteConcurrency) -> Dynamic {
  let conc_setting = case conc {
    config.ParallelWrites -> dynamic.bool(True)
    config.SerializedWrites -> dynamic.bool(False)
    config.AutomaticWrites -> dynamic_atom("auto")
  }

  dynamic.array([
    dynamic_atom("write_concurrency"),
    conc_setting,
  ])
}

/// Create the read concurrency option for ETS configuration.
fn read_concurrency_option(conc: config.ReadConcurrency) -> Dynamic {
  let conc_setting = case conc {
    config.OptimizedReads -> dynamic.bool(True)
    config.DefaultReads -> dynamic.bool(False)
  }

  dynamic.array([dynamic_atom("read_concurrency"), conc_setting])
}

fn access_option(access: config.Access) -> Dynamic {
  let atom = case access {
    config.Public -> atom.create("public")
    config.Protected -> atom.create("protected")
    config.Private -> atom.create("private")
  }
  atom.to_dynamic(atom)
}

/// From a `Config`, create the list of configuration options required by `ets:new`.
pub fn into_options(config: config.Config) -> List(Dynamic) {
  [
    dynamic_atom("set"),
    access_option(config.access),
    write_concurrency_option(config.write_concurrency),
    read_concurrency_option(config.read_concurrency),
  ]
}

// -------
// Helpers
// -------

fn dynamic_atom(a: String) -> Dynamic {
  atom.create(a) |> atom.to_dynamic
}
