import Core
import XCTest

final class SourceFileTests: XCTestCase {

  func testLocationConversion() {
    let source = testCode(
      """
      import Greetings

      public fun main() {
        print("Hello, World!")
      }
      """)

    for position in source.text.indices {
      let x = SourceLocation(file: source, index: position)
      let (line, column) = x.lineAndColumn()
      XCTAssertEqual(x, source.at(line: line, column: column))
    }
  }

}
