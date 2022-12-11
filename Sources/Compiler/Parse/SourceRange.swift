/// A half-open range of positions in a source file.
public struct SourceRange: Hashable {

  /// The source file containing the locations.
  public let source: SourceFile

  /// The start index of the range.
  public var lowerBound: String.Index { didSet { precondition(lowerBound <= upperBound) } }

  /// The end index of the range.
  public var upperBound: String.Index { didSet { precondition(lowerBound <= upperBound) } }

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
  public func first() -> SourceLocation { SourceLocation(source: source, index: lowerBound) }

  /// Returns the last source location in this range, unless the range is empty.
  public func last() -> SourceLocation? {
    lowerBound < upperBound
      ? SourceLocation(source: source, index: source.contents.index(before: upperBound)) : nil
  }

  /// Returns a copy of `self` with the upper bound set to `newUpperBound`.
  public func extended(upTo newUpperBound: String.Index) -> SourceRange {
    SourceRange(in: source, from: lowerBound, to: newUpperBound)
  }

  /// Returns a copy of `self` extended to cover `other`.
  public func extended(toCover other: SourceRange) -> SourceRange {
    precondition(source == other.source, "incompatible ranges")
    return SourceRange(
      in: source, from: Swift.min(lowerBound, other.lowerBound),
      to: Swift.max(upperBound, other.upperBound))
  }

  public static func ..< (l: SourceRange, r: SourceRange) -> SourceRange {
    precondition(l.source == r.source, "incompatible locations")
    return SourceRange(in: l.source, from: l.lowerBound, to: r.lowerBound)
  }

}

extension SourceRange: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case source, lowerBound, upperBound

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    source = try container.decode(SourceFile.self, forKey: .source)
    lowerBound = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .lowerBound), in: source.contents)
    upperBound = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .upperBound), in: source.contents)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(source, forKey: .source)
    try container.encode(lowerBound.utf16Offset(in: source.contents), forKey: .lowerBound)
    try container.encode(upperBound.utf16Offset(in: source.contents), forKey: .upperBound)
  }

}

extension SourceRange: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(
      self,
      children: [
        "start": SourceLocation(source: source, index: lowerBound),
        "end": SourceLocation(source: source, index: upperBound),
      ])
  }

}
