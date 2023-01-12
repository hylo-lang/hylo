/// A location in a source file.
public struct SourceLocation: Hashable {

  /// The source file containing the location.
  public let file: SourceFile

  /// The position of the location in the source file.
  public let index: String.Index

  /// Creates a new source location.
  public init(file: SourceFile, index: String.Index) {
    self.file = file
    self.index = index
  }

  /// The 1-based line and column indicies of this location.
  public var lineAndColumnIndices: (line: Int, column: Int) {
    file.lineAndColumnIndices(at: self)
  }

  /// Returns a source range from `l` to `r`.
  ///
  /// - Requires: `l.file == r.file`
  public static func ..< (l: Self, r: Self) -> SourceRange {
    precondition(l.file == r.file, "incompatible locations")
    return SourceRange(in: l.file, from: l.index, to: r.index)
  }

}

extension SourceLocation: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    precondition(l.file == r.file, "incompatible locations")
    return l.index < r.index
  }

}

extension SourceLocation: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case file, index

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    file = try container.decode(SourceFile.self, forKey: .file)
    index = String.Index(
      utf16Offset: try container.decode(Int.self, forKey: .index),
      in: file.text)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(file, forKey: .file)
    try container.encode(index.utf16Offset(in: file.text), forKey: .index)
  }

}

extension SourceLocation: CustomStringConvertible {

  public var description: String {
    let (line, column) = file.lineAndColumnIndices(at: self)
    return "\(file.url.relativePath):\(line):\(column)"
  }

}

extension SourceLocation: CustomReflectable {

  public var customMirror: Mirror {
    let (line, column) = file.lineAndColumnIndices(at: self)
    return Mirror(
      self,
      children: [
        "sourceURL": file.url,
        "line": line,
        "column": column,
      ])
  }

}
