import XCTest

import AST
import Basic

final class DiagnosticChecker: DiagnosticConsumer {

  init(
    context: AST.Context,
    xcFile: StaticString = #file,
    xcLine: UInt = #line
  ) {
    self.context = context
    self.xcFile = xcFile
    self.xcLine = xcLine
  }

  /// The AST context in which modules are being compiled.
  unowned let context: AST.Context

  /// The diagnostics that are expected to be received, indexed by line number.
  var expectedDiags: [Int: [DiagnosticPattern]] = [:]

  /// The file in which assertion failures are thrown.
  let xcFile: StaticString

  /// The line number at which assertion failures are thrown.
  let xcLine: UInt

  /// Scans the given source file for test annotations.
  func scan(_ source: SourceFile) {
    let lines = source.split(separator: "\n", omittingEmptySubsequences: false)
    for (i, line) in lines.enumerated() {
      guard let range = line.range(of: "#!error") else { continue }
      var start = line.index(range.lowerBound, offsetBy: 7)

      var offset: Int = 1
      if line[start...].starts(with: "@") {
        let end = line[start...].firstIndex(where: { $0.isWhitespace }) ?? line.endIndex
        offset += Int(line[line.index(after: start) ..< end]) ?? 0
        start = end
      }

      let message = line[start...].drop(while: { $0.isWhitespace })
      if message.isEmpty {
        expectedDiags[i + offset, default: []].append(DiagnosticPattern(message: nil))
      } else {
        expectedDiags[i + offset, default: []].append(DiagnosticPattern(message: String(message)))
      }
    }
  }

  func consume(_ diagnostic: Diagnostic) {
    guard let location = diagnostic.reportLocation else {
      XCTFail("unexpected diagnostic: \(diagnostic.message)", file: xcFile, line: xcLine)
      return
    }

    let source = context.sourceManager.source(containing: location)!
    let (line, column) = source.lineColumnIndices(at: location)
    guard let i = expectedDiags[line]?.firstIndex(where: { $0 ~= diagnostic }) else {
      XCTFail(
        "\(source.url.lastPathComponent):\(line):\(column): \(diagnostic.message)",
        file: xcFile, line: xcLine)
      return
    }

    expectedDiags[line]!.remove(at: i)
  }

  func finalize(_ source: SourceFile) {
    if expectedDiags.isEmpty { return }

    for (line, patterns) in expectedDiags.sorted(by: { a, b in a.key < b.key }) {
      for pattern in patterns {
        let message = pattern.message ?? "_"
        XCTFail(
          "\(source.url.lastPathComponent):\(line): missing diagnostic: \(message)",
          file: xcFile, line: xcLine)
      }
    }
  }

}
