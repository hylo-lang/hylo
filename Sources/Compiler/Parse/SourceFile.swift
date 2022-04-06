import Foundation // For `URL` and `UUID`

/// A source file.
///
/// - Note: equality between two source files is determined solely using their identifiers.
public struct SourceFile {

  /// A position in a source file.
  public typealias Index = String.Index

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

extension SourceFile: ExpressibleByStringLiteral {

  public init(stringLiteral value: String) {
    self.init(contents: value)
  }

}
