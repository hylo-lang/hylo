import XCTest
import Driver
import Eval

final class ValTests: XCTestCase {

  func testTypeChecker() throws {
    try withTestCases(in: "TestCases/TypeChecker", { (source, driver) in
      let moduleName = source.url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, sources: [source])
      driver.typeCheck(moduleDecl: moduleDecl)
      return []
    })
  }

  func testVILGen() throws {
    try withTestCases(in: "TestCases/VILGen", { (source, driver) in
      try driver.loadStdlib()
      let moduleName = source.url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, sources: [source])
      driver.typeCheck(moduleDecl: moduleDecl)

      do {
        _ = try driver.lower(moduleDecl: moduleDecl)
      } catch DriverError.loweringFailed {
      }
      return []
    })
  }

  func testEval() throws {
    try withTestCases(in: "TestCases/Eval", { (source, driver) in
      try driver.loadStdlib()
      let moduleName = source.url.deletingPathExtension().lastPathComponent
      let moduleDecl = try driver.parse(moduleName: moduleName, sources: [source])
      driver.typeCheck(moduleDecl: moduleDecl)

      var interpreter = Interpreter()
      try interpreter.load(module: driver.lower(moduleDecl: driver.compiler.stdlib!))
      try interpreter.load(module: driver.lower(moduleDecl: moduleDecl))
      XCTAssertEqual(interpreter.start(), 42)
      return []
    })
  }

}

