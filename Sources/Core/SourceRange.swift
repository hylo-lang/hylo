/// A half-open range of positions in a source file.
public struct SourceRange: Hashable {

  /// The source file containing the locations.
  public let file: SourceFile

  /// The start index of the range.
  public var start: String.Index {
    didSet { precondition(start <= end) }
  }

  /// The end index of the range.
  public var end: String.Index {
    didSet { precondition(start <= end) }
  }

  /// Creates a range in `file` from `lowerBound` to `upperBound`.
  ///
  /// - Requires: `lowerBound <= upperBound`
  public init(in file: SourceFile, from lowerBound: String.Index, to upperBound: String.Index) {
    precondition(lowerBound <= upperBound)
    self.file = file
    self.start = lowerBound
    self.end = upperBound
  }

  /// Returns whether `self` contains the given location.
  public func contains(_ l: SourceLocation) -> Bool {
    (l.file == file) && (l.index >= start) && (l.index < end)
  }

  /// Returns the first source location in this range.
  public func first() -> SourceLocation {
    file.at(start)
  }

  /// Returns the last source location in this range, unless the range is empty.
  public func last() -> SourceLocation? {
    start < end
      ? file.at(file.text.index(before: end))
      : nil
  }

  /// Returns a copy of `self` with the upper bound set to `newUpperBound`.
  public func extended(upTo newUpperBound: String.Index) -> SourceRange {
    SourceRange(in: file, from: start, to: newUpperBound)
  }

  /// Returns a copy of `self` extended to cover `other`.
  public func extended(toCover other: SourceRange) -> SourceRange {
    precondition(file == other.file, "incompatible ranges")
    return SourceRange(
      in: file,
      from: Swift.min(start, other.start),
      to: Swift.max(end, other.end))
  }

  public static func ..< (l: SourceRange, r: SourceRange) -> SourceRange {
    precondition(l.file == r.file, "incompatible locations")
    return SourceRange(in: l.file, from: l.start, to: r.start)
  }

}

extension SourceRange: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case file, lowerBound, upperBound

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    file = try container.decode(SourceFile.self, forKey: .file)
    start = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .lowerBound),
      in: file.text)
    end = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .upperBound),
      in: file.text)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(file, forKey: .file)
    try container.encode(start.utf16Offset(in: file.text), forKey: .lowerBound)
    try container.encode(end.utf16Offset(in: file.text), forKey: .upperBound)
  }

}

extension SourceRange: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(
      self,
      children: [
        "start": file.at(start),
        "end": file.at(end),
      ])
  }

}
