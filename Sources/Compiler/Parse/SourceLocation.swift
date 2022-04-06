/// A location in a source file.
public struct SouceLocation {

  /// The source file containing the location.
  public let source: SourceFile

  /// The index of the location in the source file.
  public let index: SourceFile.Index

  /// Returns a source range from `lhs` to `rhs`.
  ///
  /// - Requires: `lhs.source == rhs.source`
  public static func ..< (lhs: Self, rhs: Self) -> SourceRange {
    precondition(lhs.source == rhs.source)
    return SourceRange(in: lhs.source, from: lhs.index, to: rhs.index)
  }

}
