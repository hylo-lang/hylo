import XCTest

import AST
import Basic

final class TypeTests: XCTestCase {

  /// The AST context in which the test is being carried out.
  var context: Context!

  /// A dummy module for long lived declarations.
  var module: ModuleDecl!

  override func setUp() {
    context = Context(sourceManager: SourceManager())
    module = ModuleDecl(name: "test", generation: 0, context: context)
    module.units.append(BuiltinUnit())
    module.units[0].parentDeclSpace = module
  }

  override func tearDown() {
    context = nil
    module = nil
  }

  func testBuiltinTypeCanonical() throws {
    let i64 = try XCTUnwrap(context.getBuiltinType(named: "i64"))
    XCTAssertEqual(i64.flags, .isCanonical)
    XCTAssert(i64.canonical === i64)

    let intLiteral = try XCTUnwrap(context.getBuiltinType(named: "IntLiteral"))
    XCTAssertEqual(intLiteral.flags, .isCanonical)
    XCTAssert(intLiteral.canonical === intLiteral)
  }

  func testModuleTypeCanonical() {
    XCTAssertEqual(module.type.flags, .isCanonical)
    XCTAssertEqual(module.instanceType.flags, .isCanonical)
    XCTAssert(module.instanceType.canonical === module.instanceType)
  }

  func testNamespaceTypeCanonical() {
    let decl = NamespaceDecl(name: "A", decls: [], context: context)
    let type = decl.instanceType
    XCTAssertEqual(type.flags, .isCanonical)
    XCTAssert(type.canonical === type)
  }

  func testProductTypeCanonical() {
    let decl = ProductTypeDecl(name: "A", type: context.unresolvedType)
    let type = context.productType(decl: decl)
    XCTAssertEqual(type.flags, .isCanonical)
    XCTAssert(type.canonical === type)
  }

  func testViewTypeCanonical() {
    let decl = ViewTypeDecl(name: "V", type: context.unresolvedType)
    let type = context.viewType(decl: decl)
    XCTAssertEqual(type.flags, .isCanonical)
    XCTAssert(type.canonical === type)
  }

  func testViewCompTypeCanonical() {
    let decls = [
      ViewTypeDecl(name: "U", type: context.unresolvedType),
      ViewTypeDecl(name: "V", type: context.unresolvedType)
    ]

    let views = decls.map({ (decl) -> ViewType in
      let type = context.viewType(decl: decl)
      decl.type = type.kind
      decl.parentDeclSpace = module.units[0]
      return type
    })

    let vc0 = context.viewCompositionType([])
    XCTAssertEqual(vc0.flags, .isCanonical)
    XCTAssert(vc0.canonical === vc0)
    XCTAssert(vc0 === context.anyType)

    let vc1 = context.viewCompositionType([views[0]])
    XCTAssertEqual(vc1.flags, [])
    XCTAssert(vc1.canonical === views[0])

    // The a view composition is canonical when its elements appear in canonical order.
    let vc2 = context.viewCompositionType(views)
    XCTAssertEqual(vc2.flags, .isCanonical)
    XCTAssert(vc2.canonical === vc2)

    let vc3 = context.viewCompositionType(views.reversed())
    XCTAssertEqual(vc3.flags, [])
    XCTAssert(vc3.canonical === vc2)
  }

  func testUnionTypeCanonical() {
    let u0 = context.unionType([])
    XCTAssertEqual(u0.flags, .isCanonical)
    XCTAssert(u0.canonical === u0)
    XCTAssert(u0 === context.nothingType)

    let u1 = context.unionType([context.unitType])
    XCTAssertEqual(u1.flags, [])
    XCTAssert(u1.canonical === context.unitType)
  }

  func testTupleTypeCanonical() {
    let unitType = context.unitType
    let anyType = context.anyType
    XCTAssertEqual(unitType.flags, .isCanonical)
    XCTAssert(unitType.canonical === unitType)

    // `(a: ())` is canonical.
    let t0 = context.tupleType([TupleType.Elem(label: "a", type: unitType)])
    XCTAssertEqual(t0.flags, .isCanonical)
    XCTAssert(t0.canonical === t0)

    // `(())` is canonical.
    let t1 = context.tupleType([TupleType.Elem(label: nil, type: unitType)])
    XCTAssertEqual(t1.flags, .isCanonical)
    XCTAssert(t1.canonical === t1)

    // `(Any)` is not canonical.
    let t2 = context.tupleType([TupleType.Elem(label: nil, type: anyType)])
    XCTAssertEqual(t2.flags, [])
    XCTAssert(t2.canonical === anyType)
  }

  func testFunTypeCanonical() {
    let anyType = context.anyType
    let parenthesized = context.tupleType([TupleType.Elem(label: nil, type: anyType)])

    // `Any -> Any` is canonical.
    let p = context.funParamType(policy: .local, rawType: anyType)
    let f0 = context.funType(params: [FunType.Param(type: p)], retType: anyType)
    XCTAssertEqual(f0.flags, .isCanonical)
    XCTAssert(f0.canonical === f0)

    // `(Any) -> Any` is not canonical.
    let pp = context.funParamType(policy: .local, rawType: parenthesized)
    let f1 = context.funType(params: [FunType.Param(type: pp)], retType: anyType)
    XCTAssertEqual(f1.flags, [])
    XCTAssert(f1.canonical === f0)
  }

  func testAsyncTypeCanonical() {
    let anyType = context.anyType
    let parenthesized = context.tupleType([TupleType.Elem(label: nil, type: anyType)])

    // `async Any` is canonical.
    let at0 = context.asyncType(of: anyType)
    XCTAssertEqual(at0.flags, [.isCanonical, .hasAsync])
    XCTAssert(at0.canonical === at0)

    // `async (Any)` is not canonical.
    let at1 = context.asyncType(of: parenthesized)
    XCTAssertEqual(at1.flags, [.hasAsync])
    XCTAssert(at1.canonical === at0)
  }

}
