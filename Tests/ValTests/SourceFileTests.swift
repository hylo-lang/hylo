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
      let x = source.position(position)
      let (line, column) = x.lineAndColumn()
      XCTAssertEqual(x, source.position(line: line, column: column))
    }
  }

}
