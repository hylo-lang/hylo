import XCTest
import Driver
@testable import Compiler

final class TypeCheckingTests: XCTestCase {

  func testSpecializedTypeExpr() throws {
    let source: SourceFile = """
    view T {}
    type A<X where X: T> {}
    type B = A<Any> // #!error type 'Any' does not conform to view 'T'
    """
    try withTestCase(source, TypeCheckingTests.action)
  }

  func testSingleExprBody() throws {
    let source: SourceFile = """
    type A {}
    type B {}
    fun foo() -> Any { (a: A(), b: B()) }
    """
    try withTestCase(source, TypeCheckingTests.action)
  }

  func testMultipleStatementBody() throws {
    let source: SourceFile = """
    type A {}
    type B { var a: A }
    fun foo() -> A {
      var b = B(a: A())
      return b.a
    }
    """
    try withTestCase(source, TypeCheckingTests.action)
  }

  func testBindings() throws {
    let source: SourceFile = """
    type A {}
    type B { var a: A }
    fun foo() -> A {
      var b = B(a: A())
      return b.a
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
