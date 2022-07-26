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

extension SourceLocation: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case source, index

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    source = try container.decode(SourceFile.self, forKey: .source)
    index = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .index),
      in: source.contents)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(source, forKey: .source)
    try container.encode(index.utf16Offset(in: source.contents), forKey: .index)
  }

}
