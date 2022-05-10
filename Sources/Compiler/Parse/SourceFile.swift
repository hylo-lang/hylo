import Foundation // For `URL` and `UUID`

/// A source file.
///
/// - Note: equality between two source files is determined solely using their identifiers.
public struct SourceFile {

  /// A position in a source file.
  public typealias Position = String.Index

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

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(url)
  }

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool {
    return lhs.url == rhs.url
  }

}

extension SourceFile: Collection {

  public typealias Index = SourceLocation

  public typealias Element = String.Element

  public var startIndex: SourceLocation {
    SourceLocation(source: self, index: contents.startIndex)
  }

  public var endIndex: SourceLocation {
    SourceLocation(source: self, index: contents.endIndex)
  }

  public func index(after l: SourceLocation) -> SourceLocation {
    precondition(l.source.url == url, "location in different file")
    return SourceLocation(source: self, index: contents.index(after: l.index))
  }

  public func index(before l: SourceLocation) -> SourceLocation {
    precondition(l.source.url == url, "location in different file")
    return SourceLocation(source: self, index: contents.index(before: l.index))
  }

  public subscript(l: SourceLocation) -> Element {
    precondition(l.source.url == url, "location in different file")
    return contents[l.index]
  }

  public subscript(bounds: Range<SourceLocation>) -> Substring {
    self[bounds.lowerBound ..< bounds.upperBound]
  }

  public subscript(bounds: SourceRange) -> Substring {
    precondition(bounds.source.url == url, "location in different file")
    return contents[bounds.lowerBound ..< bounds.upperBound]
  }

}

extension SourceFile: ExpressibleByStringLiteral {

  public init(stringLiteral value: String) {
    self.init(contents: value)
  }

}
