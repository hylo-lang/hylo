import Foundation

/// A source file.
public struct SourceFile {

  private class Repr {

    let url: URL

    var contents: String

    init(url: URL, contents: String) {
      self.url = url
      self.contents = contents
    }

  }

  private let repr: Repr

  public init(url: URL) throws {
    repr = Repr(url: url, contents: try String(contentsOf: url))
  }

  public init(contents: String) {
    let url = URL(string: "buffer://\(UUID().uuidString)")!
    repr = Repr(url: url, contents: contents)
  }

  /// The URL of the file.
  public var url: URL { repr.url }

  /// The contents of the file.
  public var contents: String { repr.contents }

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

extension SourceFile: CustomStringConvertible {

  public var description: String { url.path }

}
