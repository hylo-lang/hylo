import Foundation

extension FileManager {

  /// Creates a new temporary directory and returns its URL.
  public func makeTemporaryDirectory() throws -> URL {
    let r = temporaryDirectory.appendingPathComponent(UUID().uuidString, isDirectory: true)
    try createDirectory(at: r, withIntermediateDirectories: true)
    return r
  }

  /// Returns a unique URL into which a temporary file can be written.
  public func makeTemporaryFileURL() -> URL {
    temporaryDirectory.appendingPathComponent(UUID().uuidString, isDirectory: false)
  }

}
