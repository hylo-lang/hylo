/// A location in a source file.
public struct SourceLocation: Hashable {

  /// The source file containing the location.
  public let source: SourceFile

  /// The position of the location in the source file.
  public let index: String.Index

  /// Creates a new source location.
  public init(source: SourceFile, index: String.Index) {
    self.source = source
    self.index = index
  }

  /// Returns a source range from `l` to `r`.
  ///
  /// - Requires: `l.source == r.source`
  public static func ..< (l: Self, r: Self) -> SourceRange {
    precondition(l.source == r.source, "incompatible locations")
    return SourceRange(in: l.source, from: l.index, to: r.index)
  }

}

extension SourceLocation: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    precondition(l.source == r.source, "incompatible locations")
    return l.index < r.index
  }

}
