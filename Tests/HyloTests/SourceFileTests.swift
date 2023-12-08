import Core
import XCTest
import Foundation

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

  func testUpdateSourceFileContent() throws {
    let url = URL(fileURLWithPath: "foo.hylo")
    let s1 = SourceFile(contents: "import A", fileID: url)
    XCTAssertEqual(s1.text, "import A")
    let s2 = SourceFile(contents: "import B", fileID: url)
    XCTAssertEqual(s2.text, "import B")
  }

}
