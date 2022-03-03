import XCTest
import Driver
import Eval

final class ValTests: XCTestCase {

  private func withTestCases(
    in subdirectory: String,
    _ action: (URL, inout Driver) throws -> [Diag]
  ) throws {
    let urls = try XCTUnwrap(
      Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: subdirectory),
      "No test case found")

    for url in urls {
      // Read the test case specification.
      var specParser = TestSpecParser()
      try specParser.scan(contentsOf: url)

      // Initialize the compiler driver.
      var driver = Driver()
      var checker = DiagChecker(context: driver.compiler, annotations: specParser.annotations)
      let handle = DiagDispatcher.instance.register(consumer: checker)

      // Run the test case.
      let diags: [Diag]
      do {
        diags = try action(url, &driver)
      } catch {
        diags = []
        XCTFail(error.localizedDescription)
      }

      // Compare the result of the test case with the specification.
      checker = DiagDispatcher.instance.unregister(consumer: handle) as! DiagChecker
      diags.forEach({ checker.consume($0) })
      checker.finalize()
    }
  }

  func testNameBinding() throws {
    try withTestCases(in: "TestCases/NameBinding", { (url, driver) in
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])

      var nameBinder = NameBinder(modules: driver.compiler.modules, stdlib: driver.compiler.stdlib)
      nameBinder.walk(decl: moduleDecl)
      return nameBinder.diags
    })
  }

  func testTypeChecker() throws {
    try withTestCases(in: "TestCases/TypeChecker", { (url, driver) in
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)
      return []
    })
  }

  func testVILGen() throws {
    try withTestCases(in: "TestCases/VILGen", { (url, driver) in
      try driver.loadStdlib()
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      do {
        _ = try driver.lower(moduleDecl: moduleDecl)
      } catch DriverError.loweringFailed {
      }
      return []
    })
  }

  func testEval() throws {
    try withTestCases(in: "TestCases/Eval", { (url, driver) in
      try driver.loadStdlib()
      let moduleName = url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, moduleFiles: [url])
      driver.typeCheck(moduleDecl: moduleDecl)

      var interpreter = Interpreter()
      try interpreter.load(module: driver.lower(moduleDecl: driver.compiler.stdlib!))
      try interpreter.load(module: driver.lower(moduleDecl: moduleDecl))
      XCTAssertEqual(interpreter.start(), 42)
      return []
    })
  }

}

