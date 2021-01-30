import XCTest
import Driver

final class ValTests: XCTestCase {

  func testTypeChecker() throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: "TestCases/TypeChecker"),
      "No test case found")

    for url in urls {
      var driver = Driver()
      let source = try driver.context.sourceManager.load(contentsOf: url)

      let checker = DiagnosticChecker(context: driver.context)
      driver.context.diagnosticConsumer = checker
      checker.scan(source)

      let moduleName = url.deletingPathExtension().lastPathComponent
      driver.jobs.append(CompileJob(moduleName: moduleName, moduleFiles: [url]))
      try driver.run()

      checker.finalize(source)
    }
  }

}

