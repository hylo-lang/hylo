import Core
import XCTest

extension XCTIssue {

  /// Creates an instance from a diagnostic.
  init(_ d: Diagnostic) {
    self.init(
      type: .assertionFailure,
      compactDescription: d.message,
      sourceCodeContext:
        .init(location: XCTSourceCodeLocation.init(d.site.first())))
  }

}

extension XCTSourceCodeLocation {

  /// Creates an instance from a location in a Val source file.
  convenience init(_ l: SourcePosition) {
    self.init(fileURL: l.file.url, lineNumber: l.line.index)
  }

}
