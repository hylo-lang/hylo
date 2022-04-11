import XCTest
@testable import Compiler

final class TypeCheckerTests: XCTestCase {

  func testTraitDecl() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // trait T {
    //   type X: T where X: T,        // OK
    //                   X: X,        // error: rhs is not a triat
    //                   Any == Never // error: neither type is a parameter
    //   type Y where X == Y          // OK
    //   size n where n > 0           // OK
    // }

    let trait = ast.insert(TraitDecl(
      access: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    ast[main].members.append(AnyDeclIndex(trait))

    let _Any = NameTypeExpr(
      domain: nil, identifier: SourceRepresentable(value: "Any"), arguments: [])
    let _Never = NameTypeExpr(
      domain: nil, identifier: SourceRepresentable(value: "Never"), arguments: [])

    let T = NameTypeExpr(domain: nil, identifier: SourceRepresentable(value: "T"), arguments: [])
    let X = NameTypeExpr(domain: nil, identifier: SourceRepresentable(value: "X"), arguments: [])
    let Y = NameTypeExpr(domain: nil, identifier: SourceRepresentable(value: "Y"), arguments: [])

    ast[trait].members.append(
      AnyDeclIndex(ast.insert(AssociatedTypeDecl(
        identifier: SourceRepresentable(value: "X"),
        conformances: [
          ast.insert(T)
        ],
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .conformance(
            l: ast.insert(X),
            traits: TraitComposition([
              ast.insert(T)
            ]))),
          SourceRepresentable(value: .conformance(
            l: ast.insert(X),
            traits: TraitComposition([
              ast.insert(X)
            ]))),
          SourceRepresentable(value: .equality(
            l: AnyTypeExprIndex(ast.insert(_Any)),
            r: AnyTypeExprIndex(ast.insert(_Never))))
        ])),
        defaultValue: nil))))

    ast[trait].members.append(
      AnyDeclIndex(ast.insert(AssociatedTypeDecl(
        identifier: SourceRepresentable(value: "Y"),
        conformances: [],
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .equality(
            l: AnyTypeExprIndex(ast.insert(X)),
            r: AnyTypeExprIndex(ast.insert(Y))))
        ])),
        defaultValue: nil))))

    ast[trait].members.append(
      AnyDeclIndex(ast.insert(AssociatedSizeDecl(
        identifier: SourceRepresentable(value: "n"),
        whereClause: SourceRepresentable(value: WhereClause(constraints: [
          SourceRepresentable(value: .size(
            AnyExprIndex(ast.insert(UnfoldedExpr(subexpressions: [
              AnyExprIndex(ast.insert(NameExpr(
                domain: .none,
                stem: SourceRepresentable(value: "n"),
                labels: [],
                notation: nil,
                arguments: []))),
              AnyExprIndex(ast.insert(NameExpr(
                domain: .none,
                stem: SourceRepresentable(value: ">"),
                labels: [],
                notation: .infix,
                arguments: []))),
              AnyExprIndex(ast.insert(IntLiteralExpr(value: "10"))),
            ])))
          ))
        ])),
        defaultValue: nil))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diags.count, 2)
  }

}
