import FrontEnd
import XCTest

extension XCTestCase {

  /// Returns the result of invoking `f` on an initially-empty diagnostic set, reporting any
  /// diagnostics added and/or thrown as XCTest issues.
  public func checkNoDiagnostic<R>(
    f: (inout DiagnosticSet) throws -> R, testFile: StaticString = #filePath, line: UInt = #line
  ) rethrows -> R {
    var d = DiagnosticSet()
    do {
      let r = try f(&d)
      checkEmpty(d)
      return r
    } catch let d1 as DiagnosticSet {
      XCTAssertEqual(
        d1, d, "thrown diagnostics don't match mutated diagnostics",
        file: testFile, line: line)
      checkEmpty(d)
      throw d
    }
  }

  /// Reports any diagnostics in `s` as XCTest issues.
  public func checkEmpty(_ s: DiagnosticSet) {
    for d in s.elements {
      record(XCTIssue(.error("unexpected diagnostic: '\(d.message)'", at: d.site, notes: d.notes)))
    }
  }

}

extension XCTIssue {

  /// Creates an instance from a diagnostic.
  public init(_ d: Diagnostic) {
    self.init(
      type: .assertionFailure,
      compactDescription: d.message,
      sourceCodeContext:
        .init(location: XCTSourceCodeLocation.init(d.site.start)))
  }

}

extension XCTSourceCodeLocation {

  /// Creates an instance from a location in a Hylo source file.
  fileprivate convenience init(_ l: SourcePosition) {
    self.init(fileURL: l.file.url, lineNumber: l.line.number)
  }

}
