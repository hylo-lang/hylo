/// A diagnostic related to a region of Hylo source code.
public struct Diagnostic: Hashable {

  /// The severity of a diagnostic.
  public enum Level: Hashable {

    /// A note.
    case note

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

  /// The source code or source position (if empty) identified as the cause of the error.
  public let site: SourceRange

  /// The sub-diagnostics.
  public let notes: [Diagnostic]

  /// Creates a new diagnostic.
  ///
  /// - Precondition: elements of `notes` have `self.level == .note`
  public init(level: Level, message: String, site: SourceRange, notes: [Diagnostic] = []) {
    precondition(notes.allSatisfy { $0.level == .note }, file: #filePath)
    self.level = level
    self.message = message
    self.site = site
    self.notes = notes
  }

  /// Returns a note with the given `message` highlighting `range`.
  ///
  /// - Precondition: elements of `notes` have `self.level == .note`
  public static func note(_ message: String, at site: SourceRange) -> Diagnostic {
    Diagnostic(level: .note, message: message, site: site)
  }

  /// Returns an error with the given `message` highlighting `range`.
  ///
  /// - Precondition: elements of `notes` have `self.level == .note`
  public static func error(
    _ message: String, at site: SourceRange, notes: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(level: .error, message: message, site: site, notes: notes)
  }

  /// Returns a warning with the given `message` highlighting `range`.
  ///
  /// - Precondition: elements of `notes` have `self.level == .note`
  public static func warning(
    _ message: String, at site: SourceRange, notes: [Diagnostic] = []
  ) -> Diagnostic {
    Diagnostic(level: .warning, message: message, site: site, notes: notes)
  }

}

extension Diagnostic: CustomStringConvertible {

  public var description: String {
    let prefix: String
    let l = site.first()
    let (line, column) = l.lineAndColumn
    prefix = "\(l.file.url.fileSystemPath):\(line):\(column): "
    return prefix + "\(level): \(message)"
  }

}
