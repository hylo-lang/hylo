/// An internal error due to an operation being attempted before its preconditions were satisfied.
public struct CompilerPreconditionError: Error {

  /// Creates a new compiler precondition error.
  ///
  /// - Parameters:
  ///   - message: A description of the error.
  ///   - file: The file in which the error occured. The default is the file in which this
  ///     initializer was called.
  ///   - line: The line number on which the error occured. The default is the file in which this
  ///     initializer was called.
  public init(
    message: @autoclosure () -> String = "", file: StaticString = #filePath, line: UInt = #line
  ) {
    self.message = message()
    self.file = file
    self.line = line
  }

  /// The description of the error.
  public let message: String

  /// The file in which the error occured.
  public let file: StaticString

  /// The line number on which the error occured.
  public let line: UInt

}
