import XCTest
@testable import Compiler

final class TypeCheckerTests: XCTestCase {

  func testTraitDecl() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // trait T {
    //   type X: T where X: T,        // OK
    //                   X: X,        // error: rhs is not a trait
    //                   Any == Never // error: neither type is a parameter
    //   type Y where X == Y          // OK
    //   size n where n > 0           // OK
    // }

    let trait = ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    ast[main].members.append(AnyDeclID(trait))

    ast[trait].members.append(
      AnyDeclID(ast.insert(AssociatedTypeDecl(
        identifier: SourceRepresentable(value: "X"),
        conformances: [
          ast.insertTypeName("T")
        ],
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .conformance(
            l: ast.insertTypeName("X"),
            traits: TraitComposition([
              ast.insertTypeName("T")
            ]))),
          SourceRepresentable(value: .conformance(
            l: ast.insertTypeName("X"),
            traits: TraitComposition([
              ast.insertTypeName("X")
            ]))),
          SourceRepresentable(value: .equality(
            l: AnyTypeExprID(ast.insertTypeName("Any")),
            r: AnyTypeExprID(ast.insertTypeName("Never"))))
        ])),
        defaultValue: nil))))

    ast[trait].members.append(
      AnyDeclID(ast.insert(AssociatedTypeDecl(
        identifier: SourceRepresentable(value: "Y"),
        conformances: [],
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .equality(
            l: AnyTypeExprID(ast.insertTypeName("X")),
            r: AnyTypeExprID(ast.insertTypeName("Y"))))
        ])),
        defaultValue: nil))))

    ast[trait].members.append(
      AnyDeclID(ast.insert(AssociatedSizeDecl(
        identifier: SourceRepresentable(value: "n"),
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .size(
            AnyExprID(ast.insert(UnfoldedExpr(subexpressions: [
              AnyExprID(ast.insert(NameExpr(
                domain: .none,
                stem: SourceRepresentable(value: "n"),
                labels: [],
                notation: nil,
                arguments: []))),
              AnyExprID(ast.insert(NameExpr(
                domain: .none,
                stem: SourceRepresentable(value: ">"),
                labels: [],
                notation: .infix,
                arguments: []))),
              AnyExprID(ast.insert(IntLiteralExpr(value: "10"))),
            ])))
          ))
        ])),
        defaultValue: nil))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 2)
  }

  func testMemberTypeLookup() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // trait T { public typealias A = Any }
    // typealias A = T.A

    let trait = ast.insert(TraitDecl(
      access: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    ast[main].members.append(AnyDeclID(trait))

    ast[trait].members.append(
      AnyDeclID(ast.insert(TypeAliasDecl(
        access: SourceRepresentable(value: .public),
        identifier: SourceRepresentable(value: "A"),
        genericClause: nil,
        body: SourceRepresentable(value: .typeExpr(
          AnyTypeExprID(ast.insertTypeName("Any"))))))))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      access: nil,
      identifier: SourceRepresentable(value: "A"),
      genericClause: nil,
      body: SourceRepresentable(value: .typeExpr(
        AnyTypeExprID(ast.insertTypeName("T.A"))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testAssociatedTypeLookup() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // trait T { type X }
    // trait U: T {
    //   subscript x0: Self.X { let }
    // }

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      access: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: [
        AnyDeclID(ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(value: "X"),
          conformances: [],
          whereClause: nil,
          defaultValue: nil)))
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      access: nil,
      identifier: SourceRepresentable(value: "U"),
      refinements: [ast.insertTypeName("T")],
      members: [
        AnyDeclID(ast.insert(SubscriptDecl(
          memberModifiers: [],
          identifier: SourceRepresentable(value: "x0"),
          captures: [],
          output: AnyTypeExprID(ast.insertTypeName("Self.X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ])))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testGenericTypeAlias() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // typealias Pair<X, Y> = (first: X, second: Y)
    // typealias AnyPair = Pair<Any, Any>

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      access: nil,
      identifier: SourceRepresentable(value: "Pair"),
      genericClause: SourceRepresentable(value: GenericClause(
        params: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X"),
            conformances: []))),
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "Y"),
            conformances: []))),
        ])),
      body: SourceRepresentable(value: .typeExpr(
        AnyTypeExprID(ast.insert(TupleTypeExpr(elements: [
          SourceRepresentable(value: TupleTypeExpr.Element(
            type: AnyTypeExprID(ast.insertTypeName("X")))),
          SourceRepresentable(value: TupleTypeExpr.Element(
            type: AnyTypeExprID(ast.insertTypeName("Y")))),
        ])))))))))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      identifier: SourceRepresentable(value: "AnyPair"),
      body: SourceRepresentable(value: .typeExpr(
        AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Pair"),
          arguments: [
            .type(AnyTypeExprID(ast.insertTypeName("Any"))),
            .type(AnyTypeExprID(ast.insertTypeName("Any"))),
          ])))
      ))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

}
