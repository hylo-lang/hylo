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

  func testCyclicRefinements() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // trait T: U {} // error: circular trait refinment
    // trait U: V {} // error: circular trait refinment
    // trait V: T {} // error: circular trait refinment

    // Create a fake source ranges to get different diagnostic locations.
    let file = SourceFile(contents: "tuv")
    var i = file.startIndex
    var j = file.index(after: i)

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T", range: i ..< j),
      refinements: [ast.insertTypeName("U")],
      members: []))))
    (i, j) = (j, file.index(after: j))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "U", range: i ..< j),
      refinements: [ast.insertTypeName("V")],
      members: []))))
    (i, j) = (j, file.index(after: j))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "V", range: i ..< j),
      refinements: [ast.insertTypeName("T")],
      members: []))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 3)
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
    //   subscript x0: X { let }         // OK
    //   subscript x1: Self.X { let }    // OK
    //   subscript x2: T.X { let }       // error
    //   subscript x3: Self::T.X { let } // OK
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
          output: AnyTypeExprID(ast.insertTypeName("X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          memberModifiers: [],
          identifier: SourceRepresentable(value: "x1"),
          captures: [],
          output: AnyTypeExprID(ast.insertTypeName("Self.X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          memberModifiers: [],
          identifier: SourceRepresentable(value: "x2"),
          captures: [],
          output: AnyTypeExprID(ast.insertTypeName("T.X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          memberModifiers: [],
          identifier: SourceRepresentable(value: "x2"),
          captures: [],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            domain: AnyTypeExprID(ast.insert(ConformanceLensTypeExpr(
              wrapped: AnyTypeExprID(ast.insertTypeName("Self")),
              focus: AnyTypeExprID(ast.insertTypeName("T"))))),
            identifier: SourceRepresentable(value: "X"),
            arguments: []))),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ])))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testStoredPropertyDecl() {
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))

    // type A {
    //   let x0: Any
    //   var x1: Any
    //   let (a: y0, b: y1): (a: Any, b: Never)
    //   let (a: z0, c: z1): (a: Any, b: Never) // error
    // }

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      genericClause: nil,
      conformances: [],
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          memberModifiers: [],
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .let),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x0")))))),
            annotation: AnyTypeExprID(ast.insertTypeName("Any"))))))),
        AnyDeclID(ast.insert(BindingDecl(
          memberModifiers: [],
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x1")))))),
            annotation: AnyTypeExprID(ast.insertTypeName("Any"))))))),
        AnyDeclID(ast.insert(BindingDecl(
          memberModifiers: [],
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(TuplePattern(
              elements: [
                SourceRepresentable(value: TuplePattern.Element(
                  label: "a",
                  pattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "y0")))))))),
                SourceRepresentable(value: TuplePattern.Element(
                  label: "b",
                  pattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "y1")))))))),
              ]))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
              elements: [
                SourceRepresentable(value: TupleTypeExpr.Element(
                  label: "a",
                  type: AnyTypeExprID(ast.insertTypeName("Any")))),
                SourceRepresentable(value: TupleTypeExpr.Element(
                  label: "b",
                  type: AnyTypeExprID(ast.insertTypeName("Never")))),
              ])))))))),
        AnyDeclID(ast.insert(BindingDecl(
          memberModifiers: [],
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(TuplePattern(
              elements: [
                SourceRepresentable(value: TuplePattern.Element(
                  label: "a",
                  pattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "z0")))))))),
                SourceRepresentable(value: TuplePattern.Element(
                  label: "c",
                  pattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "z1")))))))),
              ]))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
              elements: [
                SourceRepresentable(value: TupleTypeExpr.Element(
                  label: "a",
                  type: AnyTypeExprID(ast.insertTypeName("Any")))),
                SourceRepresentable(value: TupleTypeExpr.Element(
                  label: "b",
                  type: AnyTypeExprID(ast.insertTypeName("Never")))),
              ])))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
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
