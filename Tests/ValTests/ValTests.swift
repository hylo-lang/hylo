import XCTest
import Driver

final class ValTests: XCTestCase {

  func testTypeChecker() throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: "TestCases/TypeChecker"),
      "No test case found")

    for url in urls {
      let driver = Driver()
      let source = try driver.context.sourceManager.load(contentsOf: url)

      let checker = DiagnosticChecker(context: driver.context)
      driver.context.diagnosticConsumer = checker
      checker.scan(source)

      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      checker.finalize(source)
    }
  }

}

