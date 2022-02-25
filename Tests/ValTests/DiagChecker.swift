import XCTest
import Compiler

final class DiagChecker: DiagConsumer {

  /// The AST context in which modules are being compiled.
  unowned let context: Compiler

  /// The diagnostics that are expected to be received, indexed by line number.
  var diagnostics: [TestAnnotation.Location: [DiagPattern]] = [:]

  /// The file in which assertion failures are thrown.
  let xcFile: StaticString

  /// The line number at which assertion failures are thrown.
  let xcLine: UInt

  init(
    context: Compiler,
    xcFile: StaticString = #file,
    xcLine: UInt = #line
  ) {
    self.context = context
    self.xcFile = xcFile
    self.xcLine = xcLine
  }

  func insert(annotations: [TestAnnotation.Location: [TestAnnotation]]) {
    for (loc, annotations) in annotations {
      diagnostics[loc, default: []].append(contentsOf: annotations.compactMap({ a in
        switch a {
        case .diagnostic(let pattern):
          return pattern
        }
      }))
    }
  }

  func consume(_ diagnostic: Diag) {
    guard let sourceLocation = diagnostic.reportLocation else {
      XCTFail("unexpected diagnostic: \(diagnostic.message)", file: xcFile, line: xcLine)
      return
    }

    guard let source = context.sourceManager.source(containing: sourceLocation) else {
      XCTFail("unexpected source location: \(sourceLocation)", file: xcFile, line: xcLine)
      return
    }

    let (line, column) = source.lineColumnIndices(at: sourceLocation)
    let location = TestAnnotation.Location(url: source.url, line: line)

    guard let i = diagnostics[location]?.firstIndex(where: { $0 ~= diagnostic }) else {
      XCTFail(
        "\(source.url.lastPathComponent):\(line):\(column): \(diagnostic.message)",
        file: xcFile, line: xcLine)
      return
    }

    diagnostics[location]!.remove(at: i)
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
