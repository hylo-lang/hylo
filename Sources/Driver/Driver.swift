import AST
import Basic

/// Val's compiler driver, that manages the compilation process.
public struct Driver {

  /// The AST context.
  ///
  /// This is the central repository for long-lived objects (e.g., types and declarations) created
  /// throughout the compilation process.
  public let context: AST.Context

  /// The jobs to run.
  public var jobs: [Job] = []

  /// A "stack" that can be used by the jobs to store results and consume inputs.
  public var stack: [Any] = []

  public init(
    sourceManager: SourceManager? = nil,
    diagnosticConsumer: DiagnosticConsumer? = nil
  ) {
    // Create the driver's AST context.
    context = AST.Context(sourceManager: sourceManager ?? SourceManager())
    context.diagnosticConsumer = diagnosticConsumer
  }

  public mutating func run() throws {
    for job in jobs {
      try job.run(with: &self)
    }
    jobs.removeAll()
  }

}
