import Foundation

/// A wrapper type implementing `FileHandle.standardError`'s conformance to `Log`.
struct StandardErrorLog: DiagnosticLog {

  /// Indicates whether this instance supports ANSI colors.
  var hasANSIColorSupport: Bool

  /// Creates an instance.
  init() {
    self.hasANSIColorSupport = ProcessInfo.processInfo.environment["TERM"] != nil
  }

  /// Appends `text` to the stream.
  func write(_ text: String) {
    FileHandle.standardError.write(Data(text.utf8))
  }

}
