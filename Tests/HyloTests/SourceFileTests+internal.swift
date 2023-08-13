import XCTest

@testable import Core

final class SourceFileTestsInternal: XCTestCase {

  func testLineMapping() {

    let s1: SourceFile = "a\nbc\ndef"
    let s2: SourceFile = "a\nbc\ndef\n"

    var i1 = s1.text.startIndex
    var i2 = s2.text.startIndex

    func expect(
      _ i: SourceFile.Index, in s: SourceFile, line: Int, column: Int, lineText: String,
      testFile: StaticString = #filePath, testLine: UInt = #line
    ) {
      let x = s.lineAndColumn(i)

      XCTAssertEqual(x.line, s.line(containing: i).number, file: testFile, line: testLine)

      XCTAssert(
        x == (line, column),
        "\(x) == \((line, column)) failed", file: testFile, line: testLine)

      XCTAssertEqual(s.index(line: line, column: column), i)

      let t = s.line(containing: i).text
      XCTAssertEqual(String(t), lineText, file: testFile, line: testLine)
    }

    let s0: SourceFile = ""
    expect(s0.text.startIndex, in: s0, line: 1, column: 1, lineText: "")

    func advance2() {
      s1.text.formIndex(after: &i1)
      s2.text.formIndex(after: &i2)
    }

    func expectAndAdvance2(
      line: Int, column: Int, lineText: String, testFile: StaticString = #filePath,
      testLine: UInt = #line
    ) {
      expect(
        i1, in: s1, line: line, column: column, lineText: lineText,
        testFile: testFile, testLine: testLine)

      expect(
        i2, in: s2, line: line, column: column, lineText: lineText,
        testFile: testFile, testLine: testLine)
      advance2()
    }

    expectAndAdvance2(line: 1, column: 1, lineText: "a\n")
    expectAndAdvance2(line: 1, column: 2, lineText: "a\n")

    expectAndAdvance2(line: 2, column: 1, lineText: "bc\n")
    expectAndAdvance2(line: 2, column: 2, lineText: "bc\n")
    expectAndAdvance2(line: 2, column: 3, lineText: "bc\n")

    expect(i1, in: s1, line: 3, column: 1, lineText: "def")
    expect(i2, in: s2, line: 3, column: 1, lineText: "def\n")
    advance2()

    expect(i1, in: s1, line: 3, column: 2, lineText: "def")
    expect(i2, in: s2, line: 3, column: 2, lineText: "def\n")
    advance2()

    expect(i1, in: s1, line: 3, column: 3, lineText: "def")
    expect(i2, in: s2, line: 3, column: 3, lineText: "def\n")
    advance2()

    expect(i1, in: s1, line: 3, column: 4, lineText: "def")
    expect(i2, in: s2, line: 3, column: 4, lineText: "def\n")
    s2.text.formIndex(after: &i2)
    expect(i2, in: s2, line: 4, column: 1, lineText: "")

    XCTAssertEqual(i1, s1.text.endIndex)
    XCTAssertEqual(i2, s2.text.endIndex)
  }

}
