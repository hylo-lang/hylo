import XCTest
@testable import ParseGen

final class ParseGenTests: XCTestCase {

  func testReadSpec() {
    rootRelativeSpecPath
      = (#filePath.split("/").dropLast(3) + ["spec", "spec.md"]).joined(separator: "/")
    specContents = String(contentsOfFile: "/" + rootRelativeSpecPath, encoding: .utf8)
  }

}
