import Foundation

extension FileManager {

  /// Returns the URL of a temporary file.
  public func temporaryFile() -> URL {
    temporaryDirectory.appendingPathComponent("\(UUID())")
  }

}
