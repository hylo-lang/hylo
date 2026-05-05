import XCTest

/// Executes `action` and reports test failure if it does not throw `error`.
public func check<E: Error & Equatable, R>(
  throws expectedError: E, _ action: () throws->R, file: StaticString = #filePath, line: UInt = #line
) {
  XCTAssertThrowsError(try action(), file: file, line: line) {
    XCTAssertEqual($0 as? E, expectedError, "\($0)", file: file, line: line)
  }
}
