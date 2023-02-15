import Foundation
import Utils

/// A Val source file.
///
/// - Note: two source files are equal if and only if they have the same path in the filesystem, or
///   are both synthesized.
public struct SourceFile {

  /// The notional stored properties of `self`; distinguished for encoding/decoding purposes.
  ///
  /// - Note: the instance is owned by a global dictionary, `Storage.allInstances`.
  private unowned let storage: Storage

  /// A position in the source text.
  public typealias Index = String.Index

  /// The contents of the source file.
  public var text: String { storage.text }

  /// The URL of the source file.
  public var url: URL { storage.url }

  /// The start position of each line.
  fileprivate var lineStarts: [Index] { storage.lineStarts }

  /// Creates an instance representing the file at `filePath`.
  public init(contentsOf filePath: URL) throws {
    let storage = try Storage(filePath) { try String(contentsOf: filePath) }
    self.storage = storage
  }

  /// Creates a source file with the specified contents and a unique random `url`.
  public init(synthesizedText text: String) {
    let storage = Storage(URL(string: "synthesized://\(UUID().uuidString)")!) { text }
    self.storage = storage
  }

  /// The name of the source file, sans path qualification or extension.
  public var baseName: String {
    url.deletingPathExtension().lastPathComponent
  }

  /// The number of lines in the file.
  public var lineCount: Int { storage.lineStarts.count }

  /// A range covering the whole contents of this instance.
  public var wholeRange: SourceRange {
    range(text.startIndex ..< text.endIndex)
  }

  /// Returns a range starting and ending at `index`.
  public func emptyRange(at index: String.Index) -> SourceRange {
    range(index ..< index)
  }

  /// Returns the contents of the file in the specified range.
  ///
  /// - Requires: The bounds of `range` are valid positions in `self`.
  public subscript(_ range: SourceRange) -> Substring {
    precondition(range.file.url == url, "invalid range")
    return text[range.start ..< range.end]
  }

  /// Returns the position corresponding to `i` in `text`.
  ///
  /// - Requires: `i` is a valid index in `text`.
  public func position(_ i: Index) -> SourcePosition {
    SourcePosition(i, in: self)
  }

  /// Returns the position immediately before `p`.
  ///
  /// - Requires: `p` is a valid position in `self`.
  public func position(before p: SourcePosition) -> SourcePosition {
    SourcePosition(text.index(before: p.index), in: self)
  }

  /// Returns the position corresponding to the given 1-based line and column indices.
  ///
  /// - Requires: the line and column exist in `self`.
  public func position(line: Int, column: Int) -> SourcePosition {
    SourcePosition(line: line, column: column, in: self)
  }

  /// Returns the region of `self` corresponding to `r`.
  ///
  /// - Requires: `r` is a valid range in `self`.
  public func range(_ r: Range<Index>) -> SourceRange {
    SourceRange(r, in: self)
  }

  /// Returns the line containing `i`.
  ///
  /// - Requires: `i` is a valid index in `contents`.
  /// - Complexity: O(log N) where N is the number of lines in `self`.
  public func line(containing i: Index) -> SourceLine {
    SourceLine(lineStarts.partitioningIndex(where: { $0 > i }), in: self)
  }

  /// Returns the line at 1-based index `lineNumber`.
  public func line(at lineNumber: Int) -> SourceLine {
    SourceLine(lineNumber, in: self)
  }

  /// Returns the 1-based line and column numbers corresponding to `i`.
  ///
  /// - Requires: `i` is a valid index in `contents`.
  ///
  /// - Complexity: O(log N) + O(C) where N is the number of lines in `self` and C is the returned
  ///   column number.
  func lineAndColumn(_ i: Index) -> (line: Int, column: Int) {
    let lineNumber = line(containing: i).number
    let columnNumber = text.distance(from: lineStarts[lineNumber - 1], to: i) + 1
    return (lineNumber, columnNumber)
  }

  /// Returns the index in `text` corresponding to `line` and `column`.
  ///
  /// - Requires: `line` and `column` describe a valid position in `self`.
  func index(line: Int, column: Int) -> Index {
    return text.index(lineStarts[line - 1], offsetBy: column - 1)
  }

}

extension SourceFile: ExpressibleByStringLiteral {

  public init(stringLiteral text: String) {
    self.init(synthesizedText: text)
  }

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(url.scheme == "synthesized" ? URL(string: "synthesized://") : url)
  }

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool {
    return lhs.url == rhs.url
      || lhs.url.scheme == "synthesized" && rhs.url.scheme == "synthesized"
  }

}

extension SourceFile: Codable {

  /// The state that must be maintained on behalf of `SourceFile`s while they are encoded.
  struct EncodingState {

    /// Creates an empty instance.
    public init() {}

    /// A mapping from the identity of a `SourceFile`'s storage to a pair, (`id`, `s`), where `id`
    /// is the serialized representation of `SourceFile` instances having that `storage`, and `s`
    /// is the storage itself.
    fileprivate var allInstances: [ObjectIdentifier: (id: Int, storage: Storage)] = [:]

    /// Returns the corresponding state that should be used to decode the `SourceFile` values whose
    /// encoding updated the value of `self`.
    func decodingState() -> DecodingState {
      DecodingState(allInstances: allInstances.values.sorted { $0.id < $1.id }.map(\.storage))
    }

  }

  /// The state that must be maintained on behalf of `SourceFile`s while they are decoded.
  struct DecodingState: Codable {

    /// Creates an empty instance.
    ///
    /// Empty instances are stored in a `StatefulDecoder`, and are eventually replaced during the
    /// decoding of an `AST`.
    public init() { allInstances = [] }

    /// Creates an instance containing `allInstances`
    fileprivate init(allInstances: [Storage]) { self.allInstances = allInstances }

    /// The values that will be used to reconstitute `SourceFile`s.
    ///
    /// The ID serialized for a `SourceFile` is used as an index into `allInstances` to find the
    /// corresponding `storage`.
    fileprivate let allInstances: [Storage]
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let id = try container.decode(Int.self)
    storage = decoder[state: AST.DecodingState.self].allInstances[id]
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()

    try modifying(&encoder[state: AST.EncodingState.self]) { state in
      // provisional ID in case `storage` isn't found in `state`.
      var id = state.allInstances.count

      try modifying(&state.allInstances[ObjectIdentifier(storage)]) { v in
        if let v1 = v {
          id = v1.id  // found: revise the ID.
        } else {
          v = (id: id, storage: self.storage)  // not found: remember storage for later encoding.
        }
        try container.encode(id)
      }
    }

  }

}

extension SourceFile: CustomStringConvertible {

  public var description: String { "SourceFile(\(url))" }

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

extension SourceFile {

  /// The shared, immutable storage of a `SourceFile`.
  ///
  /// `unowned Storage` can be used safely anywhere, because every `Storage` instance is owned by a
  /// threadsafe global dictionary, `Storage.allInstances,` and never deallocated.
  public final class Storage: Codable, FactoryInitializable {

    /// The URL of the source file.
    fileprivate let url: URL

    /// The contents of the source file.
    fileprivate let text: String

    /// The start position of each line.
    fileprivate let lineStarts: [Index]

    /// Creates an instance with the given properties.
    private init(url: URL, text: String) {
      self.url = url
      self.text = text
      self.lineStarts = text.lineBoundaries()
    }

    /// The owner of all instances of `Storage`.
    private static var allInstances = SharedMutable<[URL: Storage]>([:])

    /// Creates an alias to the instance with the given `url` if it exists, or creates a new
    /// instance having the given `url` and the text resulting from `makeText()`.
    fileprivate convenience init(_ url: URL, makeText: () throws -> String) rethrows {
      self.init(
        aliasing: try Self.allInstances.modify { (c: inout [URL: Storage]) -> Storage in
          try modifying(&c[url]) { v in
            let r = try v ?? Storage(url: url, text: makeText())
            v = r
            return r
          }
        })
    }

    /// The data that is encoded/decoded for each instance of `self`.
    private struct Encoding: Codable {
      let url: URL
      let text: String
    }

    public required convenience init(from decoder: Decoder) throws {
      let container = try decoder.singleValueContainer()
      let e = try container.decode(Encoding.self)
      self.init(e.url) { e.text }
    }

    public func encode(to encoder: Encoder) throws {
      var container = encoder.singleValueContainer()
      try container.encode(Encoding(url: url, text: text))
    }

  }

}

extension SourceLine {

  /// The bounds of this line, including any trailing newline.
  public var bounds: SourceRange {
    let end = number < file.lineStarts.count ? file.lineStarts[number] : file.text.endIndex
    return file.range(file.lineStarts[number - 1] ..< end)
  }

}
