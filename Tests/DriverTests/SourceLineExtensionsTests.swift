import Driver
import FrontEnd
import XCTest

final class SourceLineExtensionsTests: XCTestCase {

  func testInitFromArgument() throws {
    let f = FileManager.default.makeTemporaryFileURL()
    try "Hello,\nWorld!".write(to: f, atomically: true, encoding: .utf8)

    let l1 = try XCTUnwrap(SourceLine(argument: "\(f.relativePath):1"))
    XCTAssertEqual(l1.file.url, f)
    XCTAssertEqual(l1.number, 1)

    let l2 = try XCTUnwrap(SourceLine(argument: "\(f.relativePath):2"))
    XCTAssertEqual(l2.file.url, f)
    XCTAssertEqual(l2.number, 2)

    XCTAssertNil(SourceLine(argument: "\(f.relativePath):0"))
    XCTAssertNil(SourceLine(argument: "\(f.relativePath):100"))
    XCTAssertNil(SourceLine(argument: "nowhere:1"))
  }

}
