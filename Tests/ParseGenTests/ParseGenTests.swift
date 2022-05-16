import XCTest
import Utils
@testable import ParseGen


final class ParseGenTests: XCTestCase {
  func testReadSpec() throws {
    let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
    let ebnfLines = specContents.markdownCodeLines(language: "ebnf")
    #if false
    for r in ebnfLines {
      for l in r { print(l) }
    }
    #endif
  }

}
