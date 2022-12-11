import Foundation  // For `URL` and `UUID`

/// A Val source file.
///
/// - Note: two source files are equal if and only if they have the same path in the filesystem.
public struct SourceFile {

  /// The contents of the source file.
  public let contents: String

  /// The URL of the source file.
  public let url: URL

  /// Creates a source file with the contents of the specifide URL.
  public init(contentsOf url: URL) throws {
    self.url = url
    self.contents = try String(contentsOf: url)
  }

  /// Creates a source file with the specified contents, creating a unique random URL.
  public init(contents: String) {
    self.url = URL(string: "synthesized://\(UUID().uuidString)")!
    self.contents = contents
  }

  /// Returns the contents of the file in the specified range.
  public subscript(_ range: SourceRange) -> Substring {
    precondition(range.source.url == url, "invalid source range")
    return contents[range.lowerBound..<range.upperBound]
  }

  /// The contents of the line in which `location` is defined.
  public func lineContents(at location: SourceLocation) -> Substring {
    precondition(location.source == self, "invalid location")

    var lower = location.index
    while lower > contents.startIndex {
      let predecessor = contents.index(before: lower)
      if contents[predecessor].isNewline { break } else { lower = predecessor }
    }

    var upper = location.index
    while upper < contents.endIndex && !contents[upper].isNewline {
      upper = contents.index(after: upper)
    }

    return contents[lower..<upper]
  }

  /// The 1-based line and column indices if `location`.
  public func lineAndColumnIndices(at location: SourceLocation) -> (line: Int, column: Int) {
    precondition(location.source == self, "invalid location")

    if location.index == contents.endIndex {
      let lines = contents.split(whereSeparator: { $0.isNewline })
      return (line: lines.count, column: (lines.last?.count ?? 0) + 1)
    }

    var lineIndex = 1
    for c in contents.prefix(upTo: location.index) where c.isNewline { lineIndex += 1 }

    let buffer = contents.prefix(upTo: location.index)
    var columnIndex = 1
    for c in buffer.reversed() {
      guard !c.isNewline else { break }
      columnIndex += 1
    }

    return (lineIndex, columnIndex)
  }

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) { hasher.combine(url) }

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool { return lhs.url == rhs.url }

}

extension SourceFile: ExpressibleByStringLiteral {

  public init(stringLiteral value: String) { self.init(contents: value) }

}

extension SourceFile: Codable {

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let url = try container.decode(URL.self)
    try self.init(contentsOf: url)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(url)
  }

}

extension SourceFile: CustomStringConvertible {

  public var description: String { "SourceFile(\(url)" }

}
