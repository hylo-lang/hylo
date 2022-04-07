/// An in-flight diagnostic about an error that occured at compile time.
public struct Diag {

  /// The severity of a diagnostic.
  public enum Level {

    /// An error that does not prevent compilation.
    case warning

    /// An unrecoverable error that prevents compilation.
    case error

  }

  /// A diagnostic window, providing detailed explanation about an error.
  public struct Window {

    /// The source range highlighted in the window.
    public var range: SourceRange

    /// The text of the window.
    public var text: String

  }

  /// The level of the diagnostic.
  public let level: Level

  /// The main description of the diagnostic.
  ///
  /// The message should be general and able to stand on its own.
  public let message: String

  /// The location at which the diagnostic should be reported.
  public var location: SourceLocation?

  /// The window of the diagnostic, if any.
  public var window: Window?

  /// The sub-diagnostics.
  public var children: [Diag]

}
