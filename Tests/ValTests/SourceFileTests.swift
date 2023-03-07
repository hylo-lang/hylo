import Core
import XCTest

final class SourceFileTests: XCTestCase {

  func testPositionConversion() {
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

  func testLineContaining() {
    let source: SourceFile = "1 \n 2 \n 3"
    XCTAssertEqual(source.line(containing: source.text.firstIndex(of: "1")!).number, 1)
    XCTAssertEqual(source.line(containing: source.text.firstIndex(of: "2")!).number, 2)
    XCTAssertEqual(source.line(containing: source.text.firstIndex(of: "3")!).number, 3)
    XCTAssertEqual(source.line(containing: source.text.endIndex).number, 3)
  }

}
