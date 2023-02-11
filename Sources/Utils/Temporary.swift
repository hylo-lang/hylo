import Foundation

extension FileManager {

  /// Returns the URL of a temporary directory.
  public func temporaryDirectory() throws -> URL {
    try url(
      for: .itemReplacementDirectory,
      in: .userDomainMask,
      appropriateFor: URL(
        fileURLWithPath: FileManager.default.currentDirectoryPath, isDirectory: true),
      create: true)
  }

  /// Returns the URL of a temporary file.
  public func temporaryFile() throws -> URL {
    try temporaryDirectory().appendingPathComponent(ProcessInfo().globallyUniqueString)
  }

}
