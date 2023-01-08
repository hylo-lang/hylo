import Foundation

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
      prefix = "\(l.source.url):\(line):\(column): "
    } else {
      prefix = ""
    }
    return prefix + "\(level): \(message)"
  }

}

/// A channel that accumulates reported `Diagnostic`s, optionally writing each unique one to
/// standard error when it is reported.
public struct Diagnostics {

  /// Whether reported diagnostics are to be sent immediately to the standard error stream.
  public let reportingToStderr: Bool

  /// All reported diagnostics.
  public private(set) var log: Set<Diagnostic> = []

  /// Whether an error was reported.
  public private(set) var errorReported: Bool = false

  /// Creates an instance that sends reported diagnostics to standard error iff `reportingToStderr`
  /// is `true`.
  ///
  /// - Note: the default is `false` because we build many more tests than command-line tools.
  public init(reportingToStderr: Bool = false) {
    self.reportingToStderr = reportingToStderr
  }

  /// Writes `d` into this diagnostic channel, setting `errorReported` iff `d` is an error.
  public mutating func report(_ d: Diagnostic) {
    if d.level == .error { errorReported = true }
    if log.insert(d).inserted && reportingToStderr {
      FileHandle.standardError.write(
        // When lossy conversion is allowed, result is always non-nil
        "\(d)".data(using: .utf8, allowLossyConversion: true)!)
    }
  }

  /// Reports each diagnostic in `batch`.
  public mutating func report<B: Collection<Diagnostic>>(_ batch: B) {
    for d in batch { report(d) }
  }
}

extension Diagnostics: CustomStringConvertible {

  public var description: String {
    "\(list: log.sorted(by: Diagnostic.isLoggedBefore), joinedBy: "\n")"
  }

}
