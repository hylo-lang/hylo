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
    XCTAssertEqual(i64.props, .isCanonical)
    XCTAssert(i64.canonical === i64)

    let intLiteral = try XCTUnwrap(context.getBuiltinType(named: "IntLiteral"))
    XCTAssertEqual(intLiteral.props, .isCanonical)
    XCTAssert(intLiteral.canonical === intLiteral)
  }

  func testModuleTypeCanonical() {
    XCTAssertEqual(module.type.props, .isCanonical)
    XCTAssertEqual(module.instanceType.props, .isCanonical)
    XCTAssert(module.instanceType.canonical === module.instanceType)
  }

  func testProductTypeCanonical() {
    let decl = ProductTypeDecl(name: "A", type: context.unresolvedType, range: .invalid)
    let type = context.productType(decl: decl)
    XCTAssertEqual(type.props, .isCanonical)
    XCTAssert(type.canonical === type)
  }

  func testViewTypeCanonical() {
    let decl = ViewTypeDecl(name: "V", type: context.unresolvedType, range: .invalid)
    let type = context.viewType(decl: decl)
    XCTAssertEqual(type.props, .isCanonical)
    XCTAssert(type.canonical === type)
  }

  func testViewCompTypeCanonical() {
    let decls = [
      ViewTypeDecl(name: "U", type: context.unresolvedType, range: .invalid),
      ViewTypeDecl(name: "V", type: context.unresolvedType, range: .invalid)
    ]

    let views = decls.map({ (decl) -> ViewType in
      let type = context.viewType(decl: decl)
      decl.type = type.kind
      decl.parentDeclSpace = module.units[0]
      return type
    })

    let vc0 = context.viewCompositionType([])
    XCTAssertEqual(vc0.props, .isCanonical)
    XCTAssert(vc0.canonical === vc0)
    XCTAssert(vc0 === context.anyType)

    let vc1 = context.viewCompositionType([views[0]])
    XCTAssertEqual(vc1.props, [])
    XCTAssert(vc1.canonical === views[0])

    // The a view composition is canonical when its elements appear in canonical order.
    let vc2 = context.viewCompositionType(views)
    XCTAssertEqual(vc2.props, .isCanonical)
    XCTAssert(vc2.canonical === vc2)

    let vc3 = context.viewCompositionType(views.reversed())
    XCTAssertEqual(vc3.props, [])
    XCTAssert(vc3.canonical === vc2)
  }

  func testUnionTypeCanonical() {
    let u0 = context.unionType([])
    XCTAssertEqual(u0.props, .isCanonical)
    XCTAssert(u0.canonical === u0)
    XCTAssert(u0 === context.unhabitedType)

    let u1 = context.unionType([context.unitType])
    XCTAssertEqual(u1.props, [])
    XCTAssert(u1.canonical === context.unitType)
  }

  func testTupleTypeCanonical() {
    let unit = context.unitType
    XCTAssertEqual(unit.props, .isCanonical)
    XCTAssert(unit.canonical === unit)

    // `(a: ())` is canonical.
    let t0 = context.tupleType([TupleType.Elem(label: "a", type: unit)])
    XCTAssertEqual(t0.props, .isCanonical)
    XCTAssert(t0.canonical === t0)

    // `(())` is not canonical.
    let t1 = context.tupleType([TupleType.Elem(label: nil, type: unit)])
    XCTAssertEqual(t1.props, [])
    XCTAssert(t1.canonical === unit)
  }

  func testFunTypeCanonical() {
    let unit = context.unitType  // ()
    let parenthesized = context.tupleType([TupleType.Elem(label: nil, type: unit)])  // (())

    let f0 = context.funType(paramType: unit, retType: unit)
    XCTAssertEqual(f0.props, .isCanonical)
    XCTAssert(f0.canonical === f0)

    let f1 = context.funType(paramType: parenthesized, retType: unit)
    XCTAssertEqual(f1.props, [])
    XCTAssert(f1.canonical === f0)

    let f2 = context.funType(paramType: unit, retType: parenthesized)
    XCTAssertEqual(f2.props, [])
    XCTAssert(f2.canonical === f0)
  }

  func testAsyncTypeCanonical() {
    let unit = context.unitType  // ()
    let parenthesized = context.tupleType([TupleType.Elem(label: nil, type: unit)])  // (())

    let at0 = context.asyncType(of: unit)
    XCTAssertEqual(at0.props, [.isCanonical, .hasAsync])
    XCTAssert(at0.canonical === at0)

    let at1 = context.asyncType(of: parenthesized)
    XCTAssertEqual(at1.props, [.hasAsync])
    XCTAssert(at1.canonical === at0)
  }

  func testInoutTypeCanonical() {
    let unit = context.unitType  // ()
    let parenthesized = context.tupleType([TupleType.Elem(label: nil, type: unit)])  // (())

    let io0 = context.inoutType(of: unit)
    XCTAssertEqual(io0.props, [.isCanonical, .hasInout])
    XCTAssert(io0.canonical === io0)

    let io1 = context.inoutType(of: parenthesized)
    XCTAssertEqual(io1.props, [.hasInout])
    XCTAssert(io1.canonical === io0)
  }

}
