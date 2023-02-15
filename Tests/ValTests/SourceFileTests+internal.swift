import XCTest

@testable import Core

final class SourceFileTestsInternal: XCTestCase {

  func testLineMapping() {
    let s1: SourceFile = "a\nbc\ndef"
    let s2: SourceFile = "a\nbc\ndef\n"

    var i1 = s1.text.startIndex
    var i2 = s2.text.startIndex

    func expectCorrespondence(
      _ i: SourceFile.Index, in s: SourceFile, isAt line: Int, _ column: Int,
      testFile: StaticString = #filePath, testLine: UInt = #line
    ) {
      let x = s.lineAndColumn(i)

      XCTAssert(
        x == (line, column),
        "\(x) == \((line, column)) failed", file: testFile, line: testLine)

      XCTAssertEqual(s.index(line: line, column: column), i)
    }

    func expect2(
      line: Int, column: Int, lineContents: String, testFile: StaticString = #filePath,
      testLine: UInt = #line
    ) {
      expectCorrespondence(i1, in: s1, isAt: line, column, testFile: testFile, testLine: testLine)
      expectCorrespondence(i2, in: s2, isAt: line, column, testFile: testFile, testLine: testLine)
      XCTAssertEqual(
        String(s1.lineContents(at: s1.position(i1))), lineContents, file: testFile, line: testLine)
      XCTAssertEqual(
        String(s2.lineContents(at: s2.position(i2))), lineContents, file: testFile, line: testLine)

      s1.text.formIndex(after: &i1)
      s2.text.formIndex(after: &i2)
    }

    expect2(line: 1, column: 1, lineContents: "a")
    expect2(line: 1, column: 2, lineContents: "a")

    expect2(line: 2, column: 1, lineContents: "bc")
    expect2(line: 2, column: 2, lineContents: "bc")
    expect2(line: 2, column: 3, lineContents: "bc")

    expect2(line: 3, column: 1, lineContents: "def")
    expect2(line: 3, column: 2, lineContents: "def")
    expect2(line: 3, column: 3, lineContents: "def")

    expectCorrespondence(i1, in: s1, isAt: 4, 1)

    expectCorrespondence(i2, in: s2, isAt: 3, 4)
    s2.text.formIndex(after: &i2)
    expectCorrespondence(i2, in: s2, isAt: 4, 1)
    XCTAssertEqual(i1, s1.text.endIndex)
    XCTAssertEqual(i2, s2.text.endIndex)
  }

}
