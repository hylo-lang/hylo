import Core
import XCTest

final class SourceFileTests: XCTestCase {

  func testLocationConversion() {
    let source: SourceFile = """
      import Greetings

      public fun main() {
        print("Hello, World!")
      }
      """

    for position in source.text.indices {
      let x = source.position(position)
      let (line, column) = x.lineAndColumn
      XCTAssertEqual(x, source.position(line: line, column: column))
    }
  }

  func testPositionBefore() {
    let source: SourceFile = "abc"
    let p = source.position(line: 1, column: 2)
    XCTAssertEqual(source.position(before: p), source.position(line: 1, column: 1))
  }

}
