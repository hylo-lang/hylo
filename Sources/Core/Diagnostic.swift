/// A diagnostic related to a region of Val source code.
public struct Diagnostic: Hashable {

  /// The severity of a diagnostic.
  public enum Level: Hashable {

    /// An error that does not prevent compilation.
    case warning

    /// An unrecoverable error that prevents compilation.
    case error

  }

  /// The level of the diagnostic.
  public let level: Level

  /// The main description of the diagnostic.
  ///
  /// The message should be general and able to stand on its own.
  public let message: String

  /// The location at which the diagnostic should be reported.
  public let location: SourceRange?

  /// The sub-diagnostics.
  public let children: [Diagnostic]

  /// Creates a new diagnostic.
  public init(
    level: Level,
    message: String,
    location: SourceRange? = nil,
    children: [Diagnostic] = []
  ) {
    self.level = level
    self.message = message
    self.location = location
    self.children = children
  }

  /// Creates an error diagnostic with `message` highlighting `range`.
  public static func error(
    _ message: String,
    range: SourceRange? = nil,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: message,
      location: range,
      children: children)
  }

  /// Creates a warning diagnostic with `message` highlighting `range`.
  public static func warning(
    _ message: String,
    range: SourceRange? = nil,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(
      level: .warning,
      message: message,
      location: range,
      children: children)
  }

}

extension Diagnostic: CustomStringConvertible {

  public var description: String {
    let prefix: String
    if let l = location?.first() {
      let (line, column) = l.lineAndColumnIndices
      prefix = "\(l.file.url):\(line):\(column): "
    } else {
      prefix = ""
    }
    return prefix + "\(level): \(message)"
  }

}
