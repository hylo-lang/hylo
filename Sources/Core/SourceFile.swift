import Foundation
import Utils

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

  /// The name of the source file, sans path qualification or extension.
  public var baseName: String {
    url.deletingPathExtension().lastPathComponent
  }

  /// Creates a source file with the specified contents, creating a unique random URL.
  public init(contents: String) {
    self.url = URL(string: "synthesized://\(UUID().uuidString)")!
    self.contents = contents
  }

  /// Returns the contents of the file in the specified range.
  public subscript(_ range: SourceRange) -> Substring {
    precondition(range.source.url == url, "invalid source range")
    return contents[range.lowerBound ..< range.upperBound]
  }

  /// The contents of the line in which `location` is defined.
  public func lineContents(at location: SourceLocation) -> Substring {
    precondition(location.source == self, "invalid location")

    var lower = location.index
    while lower > contents.startIndex {
      let predecessor = contents.index(before: lower)
      if contents[predecessor].isNewline {
        break
      } else {
        lower = predecessor
      }
    }

    var upper = location.index
    while upper < contents.endIndex && !contents[upper].isNewline {
      upper = contents.index(after: upper)
    }

    return contents[lower ..< upper]
  }

  /// The 1-based line and column indices if `location`.
  public func lineAndColumnIndices(at location: SourceLocation) -> (line: Int, column: Int) {
    precondition(location.source == self, "invalid location")

    if location.index == contents.endIndex {
      let lines = contents.split(whereSeparator: { $0.isNewline })
      return (line: lines.count, column: (lines.last?.count ?? 0) + 1)
    }

    var lineIndex = 1
    for c in contents.prefix(upTo: location.index) where c.isNewline {
      lineIndex += 1
    }

    let buffer = contents.prefix(upTo: location.index)
    var columnIndex = 1
    for c in buffer.reversed() {
      guard !c.isNewline else { break }
      columnIndex += 1
    }

    return (lineIndex, columnIndex)
  }

  /// Returns the location corresponding to the given 1-based line and column indices, or `nil` if
  /// these indices do not correspond to a valid location.
  public func location(at line: Int, _ column: Int) -> SourceLocation? {
    var position = contents.startIndex

    // Get to the given line.
    var currentLine = 1
    while (currentLine < line) && (position != contents.endIndex) {
      defer { position = contents.index(after: position) }
      if contents[position].isNewline {
        currentLine += 1
        if currentLine == line { break }
      }
    }

    // Make sure the line number is in bounds.
    if currentLine != line { return nil }

    // Get to the given column.
    var currentColumn = 1
    while (currentColumn < column) && (position != contents.endIndex) {
      if contents[position].isNewline { break }
      currentColumn += 1
      position = contents.index(after: position)
    }

    // Make sure the cilumn number is in bounds.
    if currentColumn != column { return nil }

    // We're done.
    return SourceLocation(source: self, index: position)
  }

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(url)
  }

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool {
    return lhs.url == rhs.url
  }

}

extension SourceFile: ExpressibleByStringLiteral {

  public init(stringLiteral value: String) {
    self.init(contents: value)
  }

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

/// Given a collection of file and directory paths as specified on the valc command line, returns
/// the actual source files to process.
///
/// Paths of files in `sourcePaths` are unconditionally treated as Val source files. Paths of
/// directories are recursively searched for `.val` files, which are considered Val `sourceFiles`;
/// all others are treated as non-source files and are ignored.
public func sourceFiles<S: Collection>(in sourcePaths: S) throws -> [SourceFile]
where S.Element == URL {
  let explicitSourcePaths = sourcePaths.filter { !$0.hasDirectoryPath }
  let sourceDirectoryPaths = sourcePaths.filter { $0.hasDirectoryPath }

  // Recursively search the directory paths, adding .val files to `sourceFiles`
  var result = try explicitSourcePaths.map(SourceFile.init)
  for d in sourceDirectoryPaths {
    try withFiles(in: d) { f in
      if f.pathExtension == "val" {
        try result.append(SourceFile(contentsOf: f))
      }
      return true
    }
  }
  return result
}
