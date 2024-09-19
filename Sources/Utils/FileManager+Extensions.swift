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

  /// Writes `s` to a temporary file and returns its URL.
  public func temporaryFile(containing s: String) throws -> URL {
    let f = self.makeTemporaryFileURL()
    try s.write(to: f, atomically: true, encoding: .utf8)
    return f
  }

}
