import XCTest

import Driver
import Eval

final class ValTests: XCTestCase {

  func testTypeChecker() throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: "TestCases/TypeChecker"),
      "No test case found")

    for url in urls {
      let driver = Driver()
      let source = try driver.context.sourceManager.load(contentsOf: url)

      let checker = DiagnosticChecker(context: driver.context)
      driver.context.diagConsumer = checker

      var parser = TestAnnotationParser()
      parser.scan(source)
      for (loc, annotations) in parser.annotations {
        checker.diagnostics[loc, default: []].append(contentsOf: annotations.compactMap({ a in
          switch a {
          case .diagnostic(let pattern):
            return pattern
          }
        }))
      }

      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      checker.finalize()
    }
  }

  func testEval() throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: "TestCases/Eval"),
      "No test case found")

    for url in urls {
      let driver = Driver()
      try driver.loadStdlib()
      let source = try driver.context.sourceManager.load(contentsOf: url)

      var parser = TestAnnotationParser()
      parser.scan(source)

      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      var interpreter = Interpreter()
      try interpreter.load(module: driver.lower(moduleDecl: driver.context.stdlib!))
      try interpreter.load(module: driver.lower(moduleDecl: moduleDecl))

      XCTAssertEqual(interpreter.start(), 42)
    }
  }

}

