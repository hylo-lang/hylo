import XCTest
import Compiler

struct DiagChecker: DiagConsumer {

  /// The AST context in which modules are being compiled.
  unowned let context: Compiler

  /// The diagnostics that are expected to be received, indexed by line number.
  var diagnostics: [TestSpec.Loc: [DiagPattern]] = [:]

  /// The file in which assertion failures are thrown.
  let xcFile: StaticString

  /// The line number at which assertion failures are thrown.
  let xcLine: UInt

  init(
    context: Compiler,
    annotations: [TestSpec.Loc: [TestSpec]],
    xcFile: StaticString = #file,
    xcLine: UInt = #line
  ) {
    self.context = context
    self.xcFile = xcFile
    self.xcLine = xcLine

    insert(annotations: annotations)
  }

  mutating func insert(annotations: [TestSpec.Loc: [TestSpec]]) {
    for (loc, annotations) in annotations {
      diagnostics[loc, default: []].append(contentsOf: annotations.compactMap({ a in
        switch a {
        case .diagnostic(let pattern):
          return pattern
        }
      }))
    }
  }

  mutating func consume(_ diag: Diag) {
    guard let diagLoc = diag.reportLocation else {
      XCTFail("unexpected diagnostic: \(diag.message)", file: xcFile, line: xcLine)
      return
    }

    let (line, column) = diagLoc.lineColumnIndices
    let specLoc = TestSpec.Loc(url: diagLoc.url, line: line)
    guard let i = diagnostics[specLoc]?.firstIndex(where: { $0 ~= diag }) else {
      XCTFail(
        "\(diagLoc.url.lastPathComponent):\(line):\(column): \(diag.message)",
        file: xcFile, line: xcLine)
      return
    }

    diagnostics[specLoc]!.remove(at: i)
  }

  func finalize() {
    if diagnostics.isEmpty { return }

    for (location, patterns) in diagnostics {
      for pattern in patterns {
        let message = pattern.message ?? "_"
        XCTFail(
          "\(location.url.lastPathComponent):\(location.line): missing diagnostic: \(message)",
          file: xcFile, line: xcLine)
      }
    }
  }

}
