import CitronLexerModule

/// An error produced at compile-time.
struct EBNFError: Error, Hashable {
  /// An additional informative note to go with the error message.
  struct Note: Hashable {
    let message: String
    let site: SourceRegion
  }

  /// A human-readable description of the problem.
  let message: String
  /// Where to point in the source code
  let site: SourceRegion
  /// Any additional notes
  let notes: [Note]

  /// Creates an instance with the given properties.
  init(_ message: String, at site: SourceRegion, notes: [Note] = []) {
    self.message = message
    self.site = site
    self.notes = notes
  }

  static func == (l: Self, r: Self) -> Bool {
    l.message == r.message && l.site == r.site
    && l.notes.lazy.map(\.message) == r.notes.lazy.map(\.message)
      && l.notes.lazy.map(\.site) == r.notes.lazy.map(\.site)
  }
}

extension EBNFError: CustomStringConvertible {
  /// String representation that, if printed at the beginning of the line,
  /// should be recognized by IDEs.
  var description: String {
    return (
      ["\(site): error: \(message)"] + notes.enumerated().lazy.map {
        (i, n) in "\(n.site): note(\(i)): \(n.message)"
      }).joined(separator: "\n")
  }
}

extension SourcePosition {
  typealias Offset = (line: Int, column: Int)

  /// Returns `l` offset by `r`
  static func + (l: Self, r: Offset) -> Self {
    return .init(line: l.line + r.line, column: l.column + r.column)
  }

  /// Returns `r` offset by `l`
  static func + (l: Offset, r: Self) -> Self {
    return .init(line: l.line + r.line, column: l.column + r.column)
  }

}

extension SourceRegion {
  /// Returns `l` offset by `r`.
  static func + (l: Self, r: SourcePosition.Offset) -> Self {
    return .init(
      fileName: l.fileName, (l.span.lowerBound + r)..<(l.span.upperBound + r))
  }

  /// Returns `r` offset by `l`.
  static func + (l: SourcePosition.Offset, r: Self) -> Self {
    return .init(
      fileName: r.fileName, (r.span.lowerBound + l)..<(r.span.upperBound + l))
  }
}

extension EBNFError {
  /// Returns `l` offset by `r`.
  static func + (l: Self, r: SourcePosition.Offset) -> Self {
    Self(
      l.message, at: l.site + r,
      notes: l.notes.map { .init(message: $0.message, site: $0.site + r) })
  }

  /// Returns `r` offset by `l`.
  static func + (l: SourcePosition.Offset, r: Self) -> Self {
    Self(
      r.message, at: r.site + l,
      notes: r.notes.map { .init(message: $0.message, site: $0.site + l) })
  }
}

typealias EBNFErrorLog = [EBNFError]
extension EBNFErrorLog: Error {}
