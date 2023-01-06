import Core
import XCTest

extension XCTIssue {

  /// Creates an instance from a diagnostic.
  init(_ d: Diagnostic) {
    self.init(
      type: .assertionFailure,
      compactDescription: d.message,
      sourceCodeContext:
        .init(location: (d.location?.first()).map(XCTSourceCodeLocation.init(_:))))
  }

}

extension XCTSourceCodeLocation {

  /// Creates an instance from a location in a Val source file.
  convenience init(_ l: SourceLocation) {
    self.init(fileURL: l.source.url, lineNumber: l.lineAndColumnIndices.line)
  }

}
