import AST

/// The description of an individual sub-process.
public protocol Job {

  /// Executes the job.
  ///
  /// - Parameter context: The AST context in which the job was ran.
  func run(in context: AST.Context) throws

}
