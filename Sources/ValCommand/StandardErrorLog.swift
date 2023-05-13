import Foundation

struct FileHandleStream: TextOutputStream {

  private let base: FileHandle

  /// Creates an instance that writes to `base`.
  init(_ base: FileHandle) { self.base = base }

  /// Appends `text` to `self`.
  public mutating func write(_ text: String) {
    base.write(Data(text.utf8))
  }

}

var standardError: any TextOutputStream = FileHandleStream(.standardError)

extension ProcessInfo {
  static let terminalIsConnected = processInfo.environment["TERM"] != nil
}
