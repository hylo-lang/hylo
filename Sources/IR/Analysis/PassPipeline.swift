import Core

/// A collection of IR passes to be applied on a module sequentially.
public struct PassPipeline {

  /// A analysis and/or transformation pass that applies `function` in `module`, accumulating
  /// diagnostics into `log`.
  public typealias Pass = (
    _ function: Function.ID,
    _ module: inout Module,
    _ log: inout Diagnostics
  ) -> Void

  /// The passes to be applied.
  public private(set) var passes: [Pass]

  /// Creates an empty pipeline.
  public init() {
    self.passes = []
  }

  /// Creates a pipeline applying the given `passes`.
  public init<S: Sequence>(_ passes: S) where S.Element == Pass {
    self.passes = Array(passes)
  }

  /// Creates a pipeline with the mandatory IR passes for modules lowered from `p`.
  public init(withMandatoryPassesForModulesLoweredFrom p: TypedProgram) {
    self.init([
      ImplicitReturnInsertionPass().run,
      DefiniteInitializationPass().run,
      LifetimePass().run,
    ])
  }

  /// Applies the passes of this instance to `m`, accumulating diagnostics into `log` and throwing
  /// if a pass reports an error.
  public func apply(_ m: inout Module, reportingDiagnosticsInto log: inout Diagnostics) throws {
    for p in passes {
      for f in m.functions.indices {
        p(f, &m, &log)
      }
      try log.throwOnError()
    }
  }

}
