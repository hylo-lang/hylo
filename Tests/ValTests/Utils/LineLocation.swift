import Foundation
import Compiler

/// A line location.
struct LineLocation: Hashable {

  /// The line at which the diagnostic occured.
  var line: Int

  /// The URL of the source file in which the diagnostic occured.
  var url: URL

  /// Creates a new line location.
  init(url: URL, line: Int) {
    self.url = url
    self.line = line
  }

  /// Creates a new line location from a source location.
  init(_ location: SourceLocation) {
    self.url = location.source.url
    (self.line, _) = location.source.lineAndColumnIndices(at: location)
  }

}
