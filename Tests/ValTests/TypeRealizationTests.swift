import XCTest
import Driver
@testable import Compiler

final class TypeRealizationTests: XCTestCase {

  func testBareName() throws {
    let source: SourceFile = """
    type A {}

    let x0: A
    let x1: Any
    let x2: Nothing
    let x3: x0 // #!error no type named 'x0' in scope
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testSpecializedName() throws {
    let source: SourceFile = """
    type A {}
    type B<T, U> {}

    let x0: B
    let x1: B<()>
    let x2: B<(), ()>

    let y0: A<()> // #!error cannot specialize non-generic type A
    let y1: B<(), (), ()> // #!error too many type arguments (expected 2, got 3)
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testCompoundName() throws {
    let source: SourceFile = """
    type A {
      type B {}
    }
    extension A.B {
      type C {}
    }

    let x0: A.B
    let x1: A.B.C
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testTuple() throws {
    let source: SourceFile = """
    type A {}

    let x0: ()
    let x1: (A)
    let x2: (a: A)
    let x3: (a: A, A)
    let x4: (a: A, b: (A, A))
    let x5: (a: A, a: A) // #!error duplicate tuple label
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testFunction() throws {
    let source: SourceFile = """
    type A {}

    let x0: () -> A
    let x1: A -> A
    let x3: A -> A -> A
    let x4: (A -> A) -> A

    let y0: (A) -> A
    let y1: (a: A) -> A
    let y2: (a: A, A) -> A
    let y3: (a: A, a: (A, A)) -> A

    let z0: (a: A, b: mut A, c: consuming A) -> A
    let z1: volatile (A) -> A
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testAsync() throws {
    let source: SourceFile = """
    type A {}

    let x0: async A
    let x2: async async A
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testUnion() throws {
    let source: SourceFile = """
    type A {}
    type B {}

    let x0: A | B
    let x1: A | A
    let x2: A | (A | B)
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  func testViewComposition() throws {
    let source: SourceFile = """
    view V {}
    view W {}

    let x0: V & W
    let x1: V & V

    // #!error@+1 view conformance requirement to non-view type '(V & W)'
    let y0: V & (V & W)
    // #!error@+1 view conformance requirement to non-view type '(V | W)'
    let y1: V & (V | W)
    """
    try withTestCase(source, TypeRealizationTests.action)
  }

  private static func action(source: SourceFile, driver: inout Driver) throws -> [Diag] {
    let moduleName = source.url.deletingPathExtension().lastPathComponent
    let moduleDecl = try driver.parse(moduleName: moduleName, sources: [source])

    var walker = Walker(
      binder: NameBinder(modules: driver.compiler.modules, stdlib: driver.compiler.stdlib),
      realizer: TypeRealizer_())
    walker.walk(decl: moduleDecl)
    return walker.binder.diags + walker.realizer.diags
  }

}

fileprivate struct Walker: NodeWalker {

  typealias Result = Bool

  var parent: Node?

  var innermostSpace: DeclSpace?

  var binder: NameBinder

  var realizer: TypeRealizer_

  mutating func willVisit(_ sign: Sign) -> (shouldWalk: Bool, nodeBefore: Sign) {
    _ = realizer.realize(sign, useSite: innermostSpace!, binder: &binder)
    return (shouldWalk: false, nodeBefore: sign)
  }

}
