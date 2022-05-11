/// A half-open range of positions in a source file.
public struct SourceRange: Hashable {

  /// The source file containing the locations.
  public let source: SourceFile

  /// The start index of the range.
  public let lowerBound: SourceFile.Position

  /// The end index of the range.
  public let upperBound: SourceFile.Position

  /// Creates a range in `source` from `lowerBound` to `upperBound`.
  ///
  /// - Requires: `lowerBound <= upperBound`
  public init(in source: SourceFile, from lowerBound: String.Index, to upperBound: String.Index) {
    precondition(lowerBound <= upperBound)
    self.source = source
    self.lowerBound = lowerBound
    self.upperBound = upperBound
  }

  /// Returns the first source location in this range.
  public func first() -> SourceLocation? {
    lowerBound < upperBound
      ? SourceLocation(source: source, index: lowerBound)
      : nil
  }

  /// Returns the last source location in this range.
  public func last() -> SourceLocation? {
    lowerBound < upperBound
      ? source.index(before: SourceLocation(source: source, index: upperBound))
      : nil
  }

}

public func ..< (l: SourceRange, r: SourceRange) -> SourceRange {
  precondition(l.source == r.source, "incompatible locations")
  return SourceRange(in: l.source, from: l.lowerBound, to: r.lowerBound)
}

public func ..< (l: SourceRange?, r: SourceRange?) -> SourceRange? {
  switch (l, r) {
  case (.some(let a), .some(let b)):
    return a ..< b
  case (.some(let a), _):
    return a
  case (_, .some(let b)):
    return b
  default:
    return nil
  }
}
