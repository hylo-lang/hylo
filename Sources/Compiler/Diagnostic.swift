/// An in-flight diagnostic about an error that occured at compile time.
public struct Diagnostic: Hashable {

  /// The severity of a diagnostic.
  public enum Level: Hashable {

    /// An error that does not prevent compilation.
    case warning

    /// An unrecoverable error that prevents compilation.
    case error

  }

  /// A diagnostic window, providing detailed explanation about an error.
  public struct Window: Hashable {

    /// The source range highlighted in the window.
    public var range: SourceRange

    /// The text of the window, if any.
    public var text: String?

    /// Creates a new diagnostic window.
    public init(range: SourceRange, text: String? = nil) {
      self.text = text
      self.range = range
    }

  }

  /// The level of the diagnostic.
  public let level: Level

  /// The main description of the diagnostic.
  ///
  /// The message should be general and able to stand on its own.
  public let message: String

  /// The location at which the diagnostic should be reported.
  public let location: SourceLocation?

  /// The window of the diagnostic, if any.
  public let window: Window?

  /// The sub-diagnostics.
  public let children: [Diagnostic]

  /// Creates a new diagnostic.
  public init(
    level: Level,
    message: String,
    location: SourceLocation? = nil,
    window: Window? = nil,
    children: [Diagnostic] = []
  ) {
    self.level = level
    self.message = message
    self.location = location
    self.window = window
    self.children = children
  }

  /// Creates an error diagnostic with `message` highlighting `range`.
  public static func error(
    _ message: String, range: SourceRange? = nil,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: message,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }), children: children)
  }

  /// Creates a warning diagnostic with `message` highlighting `range`.
  public static func warning(
    _ message: String, range: SourceRange? = nil,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(
      level: .warning,
      message: message,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }), children: children)
  }

}
