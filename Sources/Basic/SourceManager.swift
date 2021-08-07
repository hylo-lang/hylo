import Foundation

/// An object that manages the contents of source files.
///
/// A source manager is essentially a large buffer of concatenated source files. Internally, each
/// loaded file is identified by a URL that is mapped to a slice of that buffer.
public final class SourceManager {

  /// Creates a new source manager.
  public init() {
  }

  /// A dictionary mapping absolute URLs of loaded source files onto their range in the buffer.
  private var ledger: [URL: SourceFile] = [:]

  /// The manager's internal buffer.
  private var buffer: String = ""

  /// Loads a source file from the URL of a local file.
  ///
  /// - Parameters:
  ///   - url: The URL of a local file.
  ///   - encoding: The character encoding of the file at `path`.
  public func load(
    contentsOf url: URL,
    encoding: String.Encoding = .utf8
  ) throws -> SourceFile {
    let absoluteURL = url.absoluteURL
    if let sourceFile = ledger[absoluteURL] {
      return sourceFile
    }

    let startIndex = buffer.endIndex
    buffer.append(contentsOf: try String(contentsOf: url, encoding: encoding))

    let sourceFile = SourceFile(
      manager: self,
      url: url,
      startIndex: startIndex,
      endIndex: buffer.endIndex)
    ledger[absoluteURL] = sourceFile
    return sourceFile
  }

  /// Loads a source file from a local path.
  ///
  /// - Parameters:
  ///   - path: A local path.
  ///   - encoding: The character encoding of the file at `path`.
  public func load(
    contentsOf path: String,
    encoding: String.Encoding = .utf8
  ) throws -> SourceFile {
    return try load(contentsOf: URL(fileURLWithPath: path), encoding: encoding)
  }

  /// Loads a source file from a string buffer.
  ///
  /// - Parameter string: A character string with the contents of the source file.
  public func load(string: String) -> SourceFile {
    let absoluteURL = URL(string: "memory://" + UUID().uuidString)!

    let startIndex = buffer.endIndex
    buffer.append(contentsOf: string)

    let sourceFile = SourceFile(
      manager: self,
      url: absoluteURL,
      startIndex: startIndex,
      endIndex: buffer.endIndex)
    ledger[absoluteURL] = sourceFile
    return sourceFile
  }

  /// Returns the source file containing the specified location.
  ///
  /// - Parameter location: A source location.
  public func source(containing location: String.Index) -> SourceFile? {
    return ledger.values.first(where: { ($0.startIndex ..< $0.endIndex) ~= location })
  }

  /// Returns the contents of the given source file.
  ///
  /// - Parameter sourceFile: A source file.
  func contents(of sourceFile: SourceFile) -> Substring {
    precondition(sourceFile.manager === self)
    return buffer[sourceFile.startIndex ..< sourceFile.endIndex]
  }

}
