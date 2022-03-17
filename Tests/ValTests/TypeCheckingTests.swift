import XCTest
import Driver
@testable import Compiler

final class TypeCheckingTests: XCTestCase {

  func testSimple() throws {
    let source: SourceFile = """
    type A {
      let foo: Unit
    }
    """
    try withTestCase(source, TypeCheckingTests.action)
  }

  private static func action(source: SourceFile, driver: inout Driver) throws -> [Diag] {
    let moduleName = source.url.deletingPathExtension().lastPathComponent
    let moduleDecl = try driver.parse(moduleName: moduleName, sources: [source])

    var checker = TypeChecker_(modules: driver.compiler.modules, stdlib: driver.compiler.stdlib)
    XCTAssert(checker.check(decl: moduleDecl))
    return checker.diags
  }

}
