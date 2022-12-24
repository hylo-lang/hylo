import Core
import XCTest

extension XCTSourceCodeLocation {

  /// Creates an instance from a location in a Val source file.
  convenience init(_ l: SourceLocation) {
    self.init(fileURL: l.source.url, lineNumber: l.lineAndColumnIndices.line)
  }

}
