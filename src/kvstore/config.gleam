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

pub type ReadConcurrency {
  Optimized
  Default
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
