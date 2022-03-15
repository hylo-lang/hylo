import XCTest
import Compiler

struct DiagChecker: DiagConsumer {

  /// The AST context in which modules are being compiled.
  unowned let context: Compiler

  /// The diagnostics that are expected to be received, indexed by line number.
  var diagnostics: [TestSpec.Loc: [DiagPattern]] = [:]

  /// The file in which assertion failures are thrown.
  let file: StaticString

  /// The line number at which assertion failures are thrown.
  let line: UInt

  init(
    context: Compiler,
    annotations: [TestSpec.Loc: [TestSpec]],
    file: StaticString,
    line: UInt
  ) {
    self.context = context
    self.file = file
    self.line = line

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
      XCTFail("unexpected diagnostic: \(diag.message)", file: file, line: line)
      return
    }

    let (l, c) = diagLoc.lineColumnIndices
    let specLoc = TestSpec.Loc(source: diagLoc.source, line: l)
    guard let i = diagnostics[specLoc]?.firstIndex(where: { $0 ~= diag }) else {
      let path: String = !diagLoc.source.url.lastPathComponent.isEmpty
        ? "\(diagLoc.source.url.lastPathComponent):"
        : ""

      XCTFail("\(path)\(l):\(c): \(diag.message)", file: file, line: line)
      return
    }

    diagnostics[specLoc]!.remove(at: i)
  }

  func finalize() {
    if diagnostics.isEmpty { return }

    for (loc, patterns) in diagnostics {
      for pattern in patterns {
        let message = pattern.message ?? "_"
        XCTFail(
          "\(loc.source.url.lastPathComponent):\(loc.line): missing diagnostic: \(message)",
          file: file, line: line)
      }
    }
  }

}
