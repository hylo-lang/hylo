import Core
import XCTest

final class SourceFileTests: XCTestCase {

  func testLocationConversion() {
    let source = SourceFile(contents: """
    import Greetings

    public fun main() {
      print("Hello, World!")
    }
    """)

    for position in source.contents.indices {
      let location = SourceLocation(source: source, index: position)
      let (line, column) = source.lineAndColumnIndices(at: location)
      XCTAssertEqual(source.location(at: line, column), location)
    }
  }

}
