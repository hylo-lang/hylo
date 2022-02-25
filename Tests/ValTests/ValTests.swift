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
      var parser = TestSpecParser()
      try parser.scan(contentsOf: url)

      let checker = DiagChecker(context: driver.compiler)
      checker.insert(annotations: parser.annotations)

      driver.compiler.diagConsumer = checker
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      checker.finalize()
    }
  }

  func testVILGen() throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: "TestCases/VILGen"),
      "No test case found")

    for url in urls {
      let driver = Driver()
      try driver.loadStdlib()
      var parser = TestSpecParser()
      try parser.scan(contentsOf: url)

      let checker = DiagChecker(context: driver.compiler)
      checker.insert(annotations: parser.annotations)

      driver.compiler.diagConsumer = checker
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)
      do {
        _ = try driver.lower(moduleDecl: moduleDecl)
      } catch DriverError.loweringFailed {
      }

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
      var parser = TestSpecParser()
      try parser.scan(contentsOf: url)

      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      var interpreter = Interpreter()
      try interpreter.load(module: driver.lower(moduleDecl: driver.compiler.stdlib!))
      try interpreter.load(module: driver.lower(moduleDecl: moduleDecl))

      XCTAssertEqual(interpreter.start(), 42)
    }
  }

}

