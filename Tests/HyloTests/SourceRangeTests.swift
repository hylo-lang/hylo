import FrontEnd
import XCTest

// swift-format-ignore
final class SourceRangeTests: XCTestCase {
  let diagnosableSource = """
    import Greetings

    public fun main() {
      print("Hello, World!")
    }
    """.asSourceFile() // <- Moving this string literal breaks the tests below.

  let source = SourceFile(
    synthesizedText: """
      import Greetings

      public fun main() {
        print("Hello, World!")
      }
      """)

  func testGNUToStringSameLine() {
    let start = source.text.index(source.text.startIndex, offsetBy: 3)
    let end = source.text.index(start, offsetBy: 5)
    let range: SourceRange = source.range(start..<end)

    let expected = "\(source.url.relativePath):1.4-9"
    XCTAssertEqual(range.gnuStandardText, expected)
    XCTAssertEqual(range.description, expected)
  }

  func testGNUToStringDifferentLine() {
    let start = source.text.index(source.text.startIndex, offsetBy: 3)
    let end = source.text.index(start, offsetBy: 26)
    let range: SourceRange = source.range(start..<end)

    let expected = "\(source.url.relativePath):1.4-3.12"
    XCTAssertEqual(range.gnuStandardText, expected)
    XCTAssertEqual(range.description, expected)
  }

  func testDiagnosableGNUToStringSameLine() {
    let start = diagnosableSource.text.index(diagnosableSource.text.startIndex, offsetBy: 3)
    let end = diagnosableSource.text.index(start, offsetBy: 5)
    let range: SourceRange = diagnosableSource.range(start..<end)

    let expected = "\(diagnosableSource.url.relativePath):7.4-9"
    XCTAssertEqual(range.gnuStandardText, expected)
    XCTAssertEqual(range.description, expected)
  }

  func testDiagnosableGNUToStringDifferentLine() {
    let start = diagnosableSource.text.index(diagnosableSource.text.startIndex, offsetBy: 3)
    let end = diagnosableSource.text.index(start, offsetBy: 26)
    let range: SourceRange = diagnosableSource.range(start..<end)

    let expected = "\(diagnosableSource.url.relativePath):7.4-9.8"
    XCTAssertEqual(range.gnuStandardText, expected)
    XCTAssertEqual(range.description, expected)
  }

}
