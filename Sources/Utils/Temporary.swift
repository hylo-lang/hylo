import Foundation

extension FileManager {

  /// Returns the URL of a temporary file.
  public func temporaryFile() throws -> URL {
    temporaryDirectory.appendingPathComponent(ProcessInfo().globallyUniqueString)
  }

}
