import Foundation

struct OutputFileHandle: TextOutputStream {

  private let base: FileHandle

  /// Creates an instance that writes to `base`.
  init(_ base: FileHandle) { self.base = base }

  /// Appends `text` to `self`.
  public mutating func write(_ text: String) {
    base.write(Data(text.utf8))
  }

  /// Appends `data` to `self`.
  public mutating func write(_ data: Data) {
    base.write(data)
  }
}

var standardError = OutputFileHandle(.standardError)

extension ProcessInfo {
  static let terminalIsConnected = processInfo.environment["TERM"] != nil
}
