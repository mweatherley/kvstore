/// Fine-grained configuration for a key-value store.
/// 
pub type Config {
  Config(
    access: Access,
    write_concurrency: WriteConcurrency,
    read_concurrency: ReadConcurrency,
  )
}

/// The default configuration for a key-value store.
///
/// This has public read-write controls and uses the default concurrency-oriented
/// performance settings from ETS.
/// 
pub fn default() -> Config {
  Config(
    access: Public,
    write_concurrency: SerializedWrites,
    read_concurrency: DefaultReads,
  )
}

/// Performance-tuning settings for write concurrency.
///
/// Note that, regardless of the option chosen, writes are always semantically
/// concurrent.
///
/// For more information, see the [ETS documentation].
///
/// [ETS documentation]: https://www.erlang.org/doc/apps/stdlib/ets#new_2_write_concurrency
/// 
pub type WriteConcurrency {
  /// Allow writes to happen in parallel at the expense of serial write patterns
  /// and interleaved read/writes.
  /// 
  ParallelWrites

  /// The default option; writes are serialized by a global write-lock on the table.
  /// 
  SerializedWrites

  /// Let the Erlang runtime manage the write concurrency strategy used by this table.
  /// 
  AutomaticWrites
}

/// Performance-tuning settings for read concurrency.
///
/// For more information, see the [ETS documentation].
///
/// [ETS documentation]: https://www.erlang.org/doc/apps/stdlib/ets#new_2_read_concurrency
/// 
pub type ReadConcurrency {
  /// Optimize this table for many simultaneous reads. This reduces the cost of reads
  /// at the cost of making interleaved reads and writes more costly.
  /// 
  OptimizedReads

  /// Default read-concurrency tuning.
  /// 
  DefaultReads
}

/// Access controls determining the degree of table access extended to non-owner
/// processes.
/// 
pub type Access {
  /// Non-owner processes can read from and write to the table.
  /// 
  Public

  /// Non-owner processes can only read from the table.
  /// 
  Protected

  /// Non-owner processes cannot interact with the table at all.
  /// 
  Private
}
