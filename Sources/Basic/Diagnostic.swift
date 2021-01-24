/// An object that can consume and report in-flight diagnostics.
public protocol DiagnosticConsumer {

  /// Consumes and reports a diagnostic.
  ///
  /// - Parameter diagnostic: A diagnostic.
  func consume(_ diagnostic: Diagnostic)

}

/// An in-flight diagnostic about a compilation issue.
public struct Diagnostic {

  /// Creates a new in-flight diagnostic.
  ///
  /// - Parameters:
  ///   - message: The message of the diagnostic.
  public init(_ message: String, level: Level = .error) {
    self.message = message
    self.level = level
  }

  /// The message of the diagnostic.
  public let message: String

  /// The level of the diagnostic.
  public let level: Level

  /// The location at which the diagnostic should be reported.
  public var reportLocation: SourceRange.Bound?

  /// The source ranges related to this diagnostic.
  public var ranges: [SourceRange] = []

  public func set<T>(_ key: WritableKeyPath<Diagnostic, T>, value: T) -> Diagnostic {
    var copy = self
    copy[keyPath: key] = value
    return copy
  }

  /// The severity of a diagnostic.
  public enum Level {

    /// An unrecoverable error that prevents compilation.
    case error

  }

}

extension Diagnostic.Level: CustomStringConvertible {

  public var description: String {
    switch self {
    case .error: return "error"
    }
  }

}
