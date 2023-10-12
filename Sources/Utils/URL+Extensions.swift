import Foundation

extension URL {

  /// The path in the filesystem.
  ///
  /// - Precondition: `self` is a file scheme or file reference URL.
  public var fileSystemPath: String {
    autoreleasepool {
      self.standardizedFileURL.withUnsafeFileSystemRepresentation { (name: UnsafePointer<CChar>?) in
        FileManager().string(
          withFileSystemRepresentation: name!,
          length: (0...).first(where: { i in name![i] == 0 })!)
      }
    }
  }

}
