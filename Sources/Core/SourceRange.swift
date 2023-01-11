/// A half-open range of positions in a source file.
public struct SourceRange: Hashable {

  /// The source file containing the locations.
  public let file: SourceFile

  /// The start index of the range.
  public var lowerBound: String.Index {
    didSet { precondition(lowerBound <= upperBound) }
  }

  /// The end index of the range.
  public var upperBound: String.Index {
    didSet { precondition(lowerBound <= upperBound) }
  }

  /// Creates a range in `file` from `lowerBound` to `upperBound`.
  ///
  /// - Requires: `lowerBound <= upperBound`
  public init(in file: SourceFile, from lowerBound: String.Index, to upperBound: String.Index) {
    precondition(lowerBound <= upperBound)
    self.file = file
    self.lowerBound = lowerBound
    self.upperBound = upperBound
  }

  /// Returns whether `self` contains the given location.
  public func contains(_ l: SourceLocation) -> Bool {
    (l.file == file) && (l.index >= lowerBound) && (l.index < upperBound)
  }

  /// Returns the first source location in this range.
  public func first() -> SourceLocation {
    SourceLocation(file: file, index: lowerBound)
  }

  /// Returns the last source location in this range, unless the range is empty.
  public func last() -> SourceLocation? {
    lowerBound < upperBound
      ? SourceLocation(file: file, index: file.contents.index(before: upperBound))
      : nil
  }

  /// Returns a copy of `self` with the upper bound set to `newUpperBound`.
  public func extended(upTo newUpperBound: String.Index) -> SourceRange {
    SourceRange(in: file, from: lowerBound, to: newUpperBound)
  }

  /// Returns a copy of `self` extended to cover `other`.
  public func extended(toCover other: SourceRange) -> SourceRange {
    precondition(file == other.file, "incompatible ranges")
    return SourceRange(
      in: file,
      from: Swift.min(lowerBound, other.lowerBound),
      to: Swift.max(upperBound, other.upperBound))
  }

  public static func ..< (l: SourceRange, r: SourceRange) -> SourceRange {
    precondition(l.file == r.file, "incompatible locations")
    return SourceRange(in: l.file, from: l.lowerBound, to: r.lowerBound)
  }

}

extension SourceRange: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case file, lowerBound, upperBound

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    file = try container.decode(SourceFile.self, forKey: .file)
    lowerBound = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .lowerBound),
      in: file.contents)
    upperBound = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .upperBound),
      in: file.contents)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(file, forKey: .file)
    try container.encode(lowerBound.utf16Offset(in: file.contents), forKey: .lowerBound)
    try container.encode(upperBound.utf16Offset(in: file.contents), forKey: .upperBound)
  }

}

extension SourceRange: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(
      self,
      children: [
        "start": SourceLocation(file: file, index: lowerBound),
        "end": SourceLocation(file: file, index: upperBound),
      ])
  }

}
