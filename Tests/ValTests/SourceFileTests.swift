import Core
import XCTest

final class SourceFileTests: XCTestCase {

  func testLocationConversion() {
    let source = SourceFile(
      text: """
        import Greetings

        public fun main() {
          print("Hello, World!")
        }
        """)

    for position in source.text.indices {
      let location = SourceLocation(file: source, index: position)
      let (line, column) = source.lineAndColumnIndices(at: location)
      XCTAssertEqual(source.location(at: line, column), location)
    }
  }

}
