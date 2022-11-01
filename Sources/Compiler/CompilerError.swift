/// An error that occured during compilation.
public struct CompilerError: Error, CustomStringConvertible {

  /// A description of the error.
  public let description: String

  /// The diagnostics attached to the error, if any.
  public internal(set) var diagnostics: [Diagnostic] = []

}
