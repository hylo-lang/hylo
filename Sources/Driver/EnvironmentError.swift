/// An error indicating that the compiler's environment is not properly configured.
public struct EnvironmentError: Error, CustomStringConvertible {

  /// The error's message.
  public let description: String

  /// Creates an instance with given `message`.
  init(_ message: String) {
    self.description = message
  }

}
