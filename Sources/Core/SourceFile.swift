import Algorithms
import Foundation
import Utils

/// A Val source file.
///
/// - Note: two source files are equal if and only if they have the same path in the filesystem.
public struct SourceFile {

  /// The contents of the source file.
  public let text: String

  /// The URL of the source file.
  public let url: URL

  /// The start position of each line.
  ///
  /// - Invariant: always starts with `contents.startIndex` and ends with `contents.endIndex`, even
  ///   if there's no final newline.
  public let lineStarts: [String.Index]

  /// Creates a source file with the contents of the specifide URL.
  public init(contentsOf url: URL) throws {
    self.url = url
    self.text = try String(contentsOf: url)
    self.lineStarts = text.lineBoundaries()
  }

  /// The name of the source file, sans path qualification or extension.
  public var baseName: String {
    url.deletingPathExtension().lastPathComponent
  }

  /// Creates a source file with the specified contents, creating a unique random URL.
  public init(synthesizedText text: String) {
    self.url = URL(string: "synthesized://\(UUID().uuidString)")!
    self.text = text
    self.lineStarts = text.lineBoundaries()
  }

  /// Returns the contents of the file in the specified range.
  public subscript(_ range: SourceRange) -> Substring {
    precondition(range.file.url == url, "invalid source range")
    return text[range.start ..< range.end]
  }

  /// The contents of the line in which `location` is defined.
  public func lineContents(at location: SourceLocation) -> Substring {
    precondition(location.file == self, "invalid location")

    var lower = location.index
    while lower > text.startIndex {
      let predecessor = text.index(before: lower)
      if text[predecessor].isNewline {
        break
      } else {
        lower = predecessor
      }
    }

    var upper = location.index
    while upper < text.endIndex && !text[upper].isNewline {
      upper = text.index(after: upper)
    }

    return text[lower ..< upper]
  }

  /// Returns the location corresponding to the given 1-based line and column indices.
  ///
  /// - Precondition: the line and column exist in `self`.
  public func at(line: Int, column: Int) -> SourceLocation {
    return SourceLocation(file: self, line: line, column: column)
  }

  /// Returns the 1-based line and column numbers corresponding to `i`.
  ///
  /// - Precondition: `i` is a valid index in `contents`.
  func lineAndColumn(_ i: String.Index) -> (line: Int, column: Int) {
    let lineNumber = lineStarts.partitioningIndex(where: { $0 > i })
    let columnNumber = text.distance(from: lineStarts[lineNumber - 1], to: i) + 1
    return (lineNumber, columnNumber)
  }

  /// Returns the index in `text` corresponding to `line` and `column`.
  ///
  /// - Precondition: `line` and `column` describe a valid location in `self`.
  func index(line: Int, column: Int) -> String.Index {
    return text.index(lineStarts[line - 1], offsetBy: column - 1)
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
