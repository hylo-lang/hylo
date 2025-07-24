import Foundation

class OutputFileHandle: TextOutputStream, @unchecked Sendable {

  private let base: FileHandle

  /// Creates an instance that writes to `base`.
  init(_ base: FileHandle) { self.base = base }

  /// Appends `text` to `self`.
  public func write(_ text: String) {
    base.write(Data(text.utf8))
  }

  /// Appends `data` to `self`.
  public func write(_ data: Data) {
    base.write(data)
  }
}

/// An instance whose writes are directed to the standard error stream.
let standardError = OutputFileHandle(.standardError)
