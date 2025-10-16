import Foundation

class OutputFileHandle: TextOutputStream, @unchecked Sendable {
  // Sendable is satisfied by all public operations being protected by `lock`.

  private let base: FileHandle
  private let lock = NSLock()

  /// Creates an instance that writes to `base`.
  init(_ base: FileHandle) { self.base = base }

  /// Appends `text` to `self`.
  public func write(_ text: String) {
    lock.withLock {
      base.write(Data(text.utf8))
    }
  }

  /// Appends `data` to `self`.
  public func write(_ data: Data) {
    lock.withLock {
      base.write(data)
    }
  }
}

/// An instance whose writes are directed to the standard error stream.
let standardError = OutputFileHandle(.standardError)
