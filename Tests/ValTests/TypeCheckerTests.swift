import XCTest
import Compiler
import Library

final class TypeCheckerTests: XCTestCase {

  func testTraitDecl() {

    // trait T {
    //   type X: T where X: T,        // OK
    //                   X: X,        // error: rhs is not a trait
    //                   Any == Never // error: neither type is a parameter
    //   type Y where X == Y          // OK
    //   value n where n > 0          // OK
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T"),
      members: [
        AnyDeclID(ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(value: "X"),
          conformances: [
            ast.insertTypeName("T")
          ],
          whereClause: SourceRepresentable(
            value: WhereClause(
              constraints: [
                SourceRepresentable(
                  value: .conformance(
                    l: ast.insertTypeName("X"),
                    traits: TraitComposition([
                      ast.insertTypeName("T")
                    ]))),
                SourceRepresentable(
                  value: .conformance(
                    l: ast.insertTypeName("X"),
                    traits: TraitComposition([
                      ast.insertTypeName("X")
                    ]))),
                SourceRepresentable(
                  value: .equality(
                    l: ast.insertTypeName("Any"),
                    r: AnyTypeExprID(ast.insertTypeName("Never"))))
              ]))))),
        AnyDeclID(ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(value: "Y"),
          whereClause: SourceRepresentable(
            value: WhereClause(
              constraints: [
                SourceRepresentable(
                  value: .equality(
                    l: ast.insertTypeName("X"),
                    r: AnyTypeExprID(ast.insertTypeName("Y"))))
              ]))))),
        AnyDeclID(ast.insert(AssociatedValueDecl(
          identifier: SourceRepresentable(value: "n"),
          whereClause: SourceRepresentable(
            value: WhereClause(
              constraints: [
                SourceRepresentable(
                  value: .value(
                    AnyExprID(ast.insert(SequenceExpr.unfolded(
                      head: AnyExprID(ast.insert(NameExpr(
                        name: SourceRepresentable(value: "n")))),
                      tail: [
                        (
                          operator: SourceRepresentable(value: ">"),
                          rhs: AnyExprID(ast.insert(IntegerLiteralExpr(value: "10")))
                        ),
                      ])))))
              ])))))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 2)
  }

  func testCyclicRefinements() {

    // trait T: U {} // error: circular trait refinment
    // trait U: V {} // error: circular trait refinment
    // trait V: T {} // error: circular trait refinment

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    // Create a fake source ranges to get different diagnostic locations.
    let file = SourceFile(contents: "tuv")
    var i = SourceLocation(source: file, index: file.contents.startIndex)
    var j = SourceLocation(source: file, index: file.contents.index(after: i.index))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T", range: i ..< j),
      refinements: [ast.insertTypeName("U")]))))
    (i, j) = (j, SourceLocation(source: file, index: file.contents.index(after: j.index)))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "U", range: i ..< j),
      refinements: [ast.insertTypeName("V")]))))
    (i, j) = (j, SourceLocation(source: file, index: file.contents.index(after: j.index)))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "V", range: i ..< j),
      refinements: [ast.insertTypeName("T")]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 3)
  }

  func testMemberTypeLookup() {

    // trait T { public typealias A = Any }
    // typealias A = T.A

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T"),
      members: [
        AnyDeclID(ast.insert(TypeAliasDecl(
          access: SourceRepresentable(value: .public),
          identifier: SourceRepresentable(value: "A"),
          body: SourceRepresentable(
            value: .typeExpr(AnyTypeExprID(ast.insertTypeName("Any"))))))),
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      identifier: SourceRepresentable(value: "A"),
      body: SourceRepresentable(
        value: .typeExpr(AnyTypeExprID(ast.insertTypeName("T.A"))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testAssociatedTypeLookup() {

    // trait T { type X }
    // trait U: T {
    //   subscript x0: X { let }         // OK
    //   subscript x1: Self.X { let }    // OK
    //   subscript x2: T.X { let }       // error
    //   subscript x3: Self::T.X { let } // OK
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T"),
      members: [
        AnyDeclID(ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(value: "X"))))
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "U"),
      refinements: [ast.insertTypeName("T")],
      members: [
        AnyDeclID(ast.insert(SubscriptDecl(
          identifier: SourceRepresentable(value: "x0"),
          output: AnyTypeExprID(ast.insertTypeName("X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          identifier: SourceRepresentable(value: "x1"),
          output: AnyTypeExprID(ast.insertTypeName("Self.X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          identifier: SourceRepresentable(value: "x2"),
          output: AnyTypeExprID(ast.insertTypeName("T.X")),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ]))),
        AnyDeclID(ast.insert(SubscriptDecl(
          identifier: SourceRepresentable(value: "x2"),
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            domain: AnyTypeExprID(ast.insert(ConformanceLensTypeExpr(
              wrapped: AnyTypeExprID(ast.insertTypeName("Self")),
              focus: AnyTypeExprID(ast.insertTypeName("T"))))),
            identifier: SourceRepresentable(value: "X")))),
          impls: [
            ast.insert(SubscriptImplDecl(introducer: SourceRepresentable(value: .let)))
          ])))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testStoredPropertyDecl() {

    // type A {
    //   let x0: Any
    //   var x1: Any
    //   let (a: y0, b: y1): (a: Any, b: Never)
    //   let (a: z0, c: z1): (a: Any, b: Never) // error
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .let),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x0")))))),
            annotation: AnyTypeExprID(ast.insertTypeName("Any"))))))),
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x1")))))),
            annotation: AnyTypeExprID(ast.insertTypeName("Any"))))))),
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(TuplePattern(
              elements: [
                SourceRepresentable(
                  value: TuplePattern.Element(
                    label: "a",
                    pattern: AnyPatternID(ast.insert(NamePattern(
                      decl: ast.insert(VarDecl(
                        identifier: SourceRepresentable(value: "y0")))))))),
                SourceRepresentable(
                  value: TuplePattern.Element(
                    label: "b",
                    pattern: AnyPatternID(ast.insert(NamePattern(
                      decl: ast.insert(VarDecl(
                        identifier: SourceRepresentable(value: "y1")))))))),
              ]))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
              elements: [
                TupleTypeExpr.Element(
                  label: SourceRepresentable(value: "a"),
                  type: AnyTypeExprID(ast.insertTypeName("Any"))),
                TupleTypeExpr.Element(
                  label: SourceRepresentable(value: "b"),
                  type: AnyTypeExprID(ast.insertTypeName("Never"))),
              ])))))))),
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(TuplePattern(
              elements: [
                SourceRepresentable(
                  value: TuplePattern.Element(
                    label: "a",
                    pattern: AnyPatternID(ast.insert(NamePattern(
                      decl: ast.insert(VarDecl(
                        identifier: SourceRepresentable(value: "z0")))))))),
                SourceRepresentable(
                  value: TuplePattern.Element(
                    label: "c",
                    pattern: AnyPatternID(ast.insert(NamePattern(
                      decl: ast.insert(VarDecl(
                        identifier: SourceRepresentable(value: "z1")))))))),
              ]))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
              elements: [
                TupleTypeExpr.Element(
                  label: SourceRepresentable(value: "a"),
                  type: AnyTypeExprID(ast.insertTypeName("Any"))),
                TupleTypeExpr.Element(
                  label: SourceRepresentable(value: "b"),
                  type: AnyTypeExprID(ast.insertTypeName("Never"))),
              ])))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testMethodExternalLookup() {

    // fun main() {
    //   let _ = 0.copy()
    // }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "main"),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(FunCallExpr(
                  callee: AnyExprID(ast.insert(NameExpr(
                    domain: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "0")))),
                    name: SourceRepresentable(value: "copy")))))))))))))
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testMethodInternalLookup() {

    // type A {
    //   fun foo() { bar() }
    //   fun bar() { self.foo() }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "foo"),
          body: SourceRepresentable(value: .expr(AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              name: SourceRepresentable(value: "bar")))))))))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "bar"),
          body: SourceRepresentable(value: .expr(AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              domain: .expr(AnyExprID(ast.insert(NameExpr(
                name: SourceRepresentable(value: "self"))))),
              name: SourceRepresentable(value: "foo")))))))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testMethodImplLookup() {

    // type A {
    //   fun foo(_ x: A) -> A {
    //     let { A() }
    //     sink { A() }
    //   }
    //   fun bar() { _ = foo.let(self) }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "foo"),
          parameters: [
            ast.insert(ParameterDecl(
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "A")))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "A")))),
          body: SourceRepresentable(value: .bundle([
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .let),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .sink),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
          ]))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "bar"),
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt(
              stmts: [
                AnyStmtID(ast.insert(DiscardStmt(
                  expr: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      name: SourceRepresentable(value: Name(
                        stem: "foo",
                        introducer: .let))))),
                    arguments: [
                      CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          name: SourceRepresentable(value: "self"))))),
                    ])))))),
              ]))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testBindingTypeInference() {

    // let x0 = ()
    // let (y0, y1) = ((), ())

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "x0")))))))),
      initializer: AnyExprID(ast.insert(TupleExpr()))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(TuplePattern(
          elements: [
            SourceRepresentable(
              value: TuplePattern.Element(
                pattern: AnyPatternID(ast.insert(NamePattern(
                  decl: ast.insert(VarDecl(
                    identifier: SourceRepresentable(value: "y0"))))
                )))),
            SourceRepresentable(
              value: TuplePattern.Element(
                pattern: AnyPatternID(ast.insert(NamePattern(
                  decl: ast.insert(VarDecl(
                    identifier: SourceRepresentable(value: "y1"))))
                )))),
          ]))))),
      initializer: AnyExprID(ast.insert(TupleExpr(
        elements: [
          TupleExpr.Element(
            value: AnyExprID(ast.insert(TupleExpr()))),
          TupleExpr.Element(
            value: AnyExprID(ast.insert(TupleExpr()))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testBindingTypeInferenceWithHints() {

    // let x0: (Int, Double) = (2, 3)
    // let x1: (_, Double) = (2, 3)

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "x0")))))),
        annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
          elements: [
            TupleTypeExpr.Element(
              type: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Int"))))),
            TupleTypeExpr.Element(
              type: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Double"))))),
          ]))))),
      initializer: AnyExprID(ast.insert(TupleExpr(
        elements: [
          TupleExpr.Element(
            value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "2")))),
          TupleExpr.Element(
            value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "3")))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "x0")))))),
        annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
          elements: [
            TupleTypeExpr.Element(
              type: AnyTypeExprID(ast.insert(WildcardTypeExpr()))),
            TupleTypeExpr.Element(
              type: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Double"))))),
          ]))))),
      initializer: AnyExprID(ast.insert(TupleExpr(
        elements: [
          TupleExpr.Element(
            value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "2")))),
          TupleExpr.Element(
            value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "3")))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testMissingBindingAnnotation() {

    // let x // error

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "x"))))))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testTrivialFunction() {

    // fun f0() {}
    // fun f1() -> () {}

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f0"),
      parameters: [],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f1"),
      parameters: [],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testDefaultFunctionArgument() {

    // fun f0(_ a: (), _ b: () = ()) {}

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f0"),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "a"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "b"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))),
          defaultValue: AnyExprID(ast.insert(TupleExpr())))),
      ],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testDuplicateFunctionParameter() {

    // fun f0(_ a: (), _ a: ()) {} // error: duplicate parameter name 'a'

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f0"),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "a"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "a"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))),
      ],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testExpressionBodiedFunction() {

    // fun forty_two() -> Int { 42 }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "forty_two"),
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "Int")))),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "42")))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testBraceBodyFunction() {

    // fun no_op() { return }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "no_op"),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(ReturnStmt())),
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testSinkCapturingFunction() {

    // fun f0[sink let x = ()]() {}

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f0"),
      captures: [
        ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .sinklet),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x")))))))),
          initializer: AnyExprID(ast.insert(TupleExpr())))),
      ],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testIdentityFunction() {

    // fun identity<T>(_ x: sink T) -> T { x }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "identity"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "T")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "T"))))))))
      ],
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "T")))),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "x"))))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testSimpleFunctionCall() {

    // fun f(_ x: sink ()) -> () { x }
    // let _ = f(())

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr())))
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testGenericFunctionCall() {

    // fun f<X, Y>(_ x: sink X, _ y: sink Y) -> X { x }
    // let _ = f((a0: ()), (b0: ()))
    // let _ = f((a1: ()), (b1: ()))

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "Y")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "X")))))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "y"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Y")))))))),
      ],
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "X")))),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "a0"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "b0"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "a1"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "b1"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testGenericFunctionCallWithDependentParameters() {

    // fun f<X>(_ x0: sink X, _ x1: sink (b: X)) {}
    // let _ = f((), (b: ()))      // OK
    // let _ = f((a: ()), (b: ())) // error

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x0"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "X")))))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x1"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr(elements: [
              TupleTypeExpr.Element(
                label: SourceRepresentable(value: "b"),
                type: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "X")))))
            ]))))))),
      ],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt())))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr()))),
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "b"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "a"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                TupleExpr.Element(
                  label: SourceRepresentable(value: "b"),
                  value: AnyExprID(ast.insert(TupleExpr()))),
              ])))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssert(checker.diagnostics.count == 1)
  }

  func testWrongArityInFunctionCall() {

    // fun f(_ x: sink ()) -> () { x }
    // let _ = f()       // error
    // let _ = f((), ()) // error

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr()))),
          CallArgument(
            value: AnyExprID(ast.insert(TupleExpr()))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssert(checker.diagnostics.count == 2)
  }

  func testWrongLabelInFunctionCall() {

    // fun f(x: sink ()) -> () { x }
    // let _ = f(y: ()) // error

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      parameters: [
        ast.insert(ParameterDecl(
          label: SourceRepresentable(value: "x"),
          identifier: SourceRepresentable(value: "x"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "f")))),
        arguments: [
          CallArgument(
            label: SourceRepresentable(value: "y"),
            value: AnyExprID(ast.insert(TupleExpr())))
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssert(checker.diagnostics.count == 1)
  }

  func testIllegalMemberwiseInitDecl() {

    // memberwise init

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .memberwiseInit),
      accessModifier: SourceRepresentable(value: .public)))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssert(checker.diagnostics.count == 1)
  }

  func testMemberwiseInitCall() {

    // type A {
    //   var foo: ()
    //   var bar: ()
    //   public memberwise init
    // }
    // let _ = A(foo: (), bar: ())

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "foo")))))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))),
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "bar")))))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .memberwiseInit),
          accessModifier: SourceRepresentable(value: .public)))),
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "A")))),
        arguments: [
          CallArgument(
            label: SourceRepresentable(value: "foo"),
            value: AnyExprID(ast.insert(TupleExpr()))),
          CallArgument(
            label: SourceRepresentable(value: "bar"),
            value: AnyExprID(ast.insert(TupleExpr()))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testImplicitMemberwiseInitCall() {

    // @implicitpublic
    // let _ = A()
    // type A {}
    //
    // Note: The call to `A`'s initializer appears before `A`'s declaration, thus requiring the
    // initializer's declaration to be hoisted.

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "A")))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A")))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testGenericMemberwiseInitCall() {

    // type A<X, Y> {
    //   var foo: X
    //   var bar: Y
    //   public memberwise init
    // }
    // let _ = A(foo: (), bar: ())

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "Y")))),
        ])),
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "foo")))))),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "X"))))))))),
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "bar")))))),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Y"))))))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .memberwiseInit),
          accessModifier: SourceRepresentable(value: .public)))),
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "A")))),
        arguments: [
          CallArgument(
            label: SourceRepresentable(value: "foo"),
            value: AnyExprID(ast.insert(TupleExpr()))),
          CallArgument(
            label: SourceRepresentable(value: "bar"),
            value: AnyExprID(ast.insert(TupleExpr()))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testUserInitCall() {

    // type A {
    //   var x: ()
    //   public init(value: sink ()) { self = A(x: value) }
    // }
    // let _ = A(value: ())

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x")))))),
            annotation: AnyTypeExprID(ast.insert(TupleTypeExpr()))))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .`init`),
          accessModifier: SourceRepresentable(value: .public),
          parameters: [
            ast.insert(ParameterDecl(
              label: SourceRepresentable(value: "value"),
              identifier: SourceRepresentable(value: "value"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))),
          ],
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt(
              stmts: [
                AnyStmtID(ast.insert(ExprStmt(
                  expr: AnyExprID(ast.insert(AssignExpr(
                    left: AnyExprID(ast.insert(NameExpr(
                      name: SourceRepresentable(value: "self")))),
                    right: AnyExprID(ast.insert(FunCallExpr(
                      callee: AnyExprID(ast.insert(NameExpr(
                        name: SourceRepresentable(value: "A")))),
                      arguments: [
                        CallArgument(
                          label: SourceRepresentable(value: "x"),
                          value: AnyExprID(ast.insert(TupleExpr()))),
                      ])))))))))
              ]))))))),
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "A")))),
        arguments: [
          CallArgument(
            label: SourceRepresentable(value: "value"),
            value: AnyExprID(ast.insert(TupleExpr()))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testMethodBundleDecl() {

    // type A {
    //   fun foo(_ x: A) -> A {
    //     let   { A() }
    //     inout { () }
    //     sink  { A() }
    //   }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "foo"),
          parameters: [
            ast.insert(ParameterDecl(
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "A")))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "A")))),
          body: SourceRepresentable(value: .bundle([
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .let),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .inout),
              body: .expr(AnyExprID(ast.insert(TupleExpr()))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .sink),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
          ])))))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testInvalidMethodBundleType() {

    // type A {
    //   fun foo(_ x: A) -> () {  // error: invalid return type for inout-capable method bundle
    //     inout { () }
    //   }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "foo"),
          parameters: [
            ast.insert(ParameterDecl(
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "A")))))))),
          ],
          output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
          body: SourceRepresentable(value: .bundle([
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .inout),
              body: .expr(AnyExprID(ast.insert(TupleExpr()))))),
          ])))))
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testMethodBundleCall() {

    // type A {
    //   fun foo(_ x: A) -> A {
    //     let { A() }
    //     sink { A() }
    //   }
    //   fun bar() { _ = foo(self) }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "foo"),
          parameters: [
            ast.insert(ParameterDecl(
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "A")))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "A")))),
          body: SourceRepresentable(value: .bundle([
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .let),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .sink),
              body: .expr(AnyExprID(ast.insert(FunCallExpr(
                callee: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "A")))))))))),
          ]))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          identifier: SourceRepresentable(value: "bar"),
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt(
              stmts: [
                AnyStmtID(ast.insert(DiscardStmt(
                  expr: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      name: SourceRepresentable(value: "foo")))),
                    arguments: [
                      CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          name: SourceRepresentable(value: "self"))))),
                    ])))))),
              ]))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testInoutExpr() {

    // type A {
    //   inout fun foo() {}
    // }
    // fun main() {
    //   var a = A()
    //   &a.foo()
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      identifier: SourceRepresentable(value: "A"),
      members: [
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          memberModifiers: [
            SourceRepresentable(value: .receiver(.inout)),
          ],
          identifier: SourceRepresentable(value: "foo"),
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt())))))),
      ]))))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "main"),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .var),
                  subpattern: AnyPatternID(ast.insert(
                    NamePattern(decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "a")))))))),
                initializer: AnyExprID(ast.insert(FunCallExpr(
                  callee: AnyExprID(ast.insert(NameExpr(
                    name: SourceRepresentable(value: "A"))))))))))))),
            AnyStmtID(ast.insert(ExprStmt(
              expr: AnyExprID(ast.insert(InoutExpr(
                subexpr: AnyExprID(ast.insert(FunCallExpr(
                  callee: AnyExprID(ast.insert(NameExpr(
                    domain: .expr(AnyExprID(ast.insert(NameExpr(
                      name: SourceRepresentable(value: "a"))))),
                    name: SourceRepresentable(value: "foo"))))))))))))),
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testGenericTypeAlias() {

    // typealias Pair<X, Y> = (first: X, second: Y)
    // typealias AnyPair = Pair<Any, Any>

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      identifier: SourceRepresentable(value: "Pair"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "Y")))),
        ])),
      body: SourceRepresentable(
        value: .typeExpr(
          AnyTypeExprID(ast.insert(TupleTypeExpr(
            elements: [
              TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insertTypeName("X"))),
              TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insertTypeName("Y"))),
            ])))))))))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      identifier: SourceRepresentable(value: "AnyPair"),
      body: SourceRepresentable(
        value: .typeExpr(
          AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Pair"),
            arguments: [
              GenericArgument(value: .type(AnyTypeExprID(ast.insertTypeName("Any")))),
              GenericArgument(value: .type(AnyTypeExprID(ast.insertTypeName("Any")))),
            ])))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testIntegerLiteralExpr() {

    // 42

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    let expr = AnyExprID(ast.insert(IntegerLiteralExpr(value: "42")))

    // Infer the type of the literal without any contextual information.
    do {
      var checker = TypeChecker(ast: ast)
      let intType = ProductType(standardLibraryTypeNamed: "Int", ast: ast)!
      let type = checker.infer(expr: expr, inScope: main)
      XCTAssertEqual(type, .product(intType))
    }

    // Infer the type of the literal assuming it's `Double` from the context.
    do {
      var checker = TypeChecker(ast: ast)
      let doubleType = ProductType(standardLibraryTypeNamed: "Double", ast: ast)!
      let type = checker.infer(expr: expr, expectedType: .product(doubleType), inScope: main)
      XCTAssertEqual(type, .product(doubleType))
    }

    // Infer the type of the literal assuming its `()` from the context.
    do {
      var checker = TypeChecker(ast: ast)
      let type = checker.infer(expr: expr, expectedType: .unit, inScope: main)
      XCTAssertNil(type)
      XCTAssert(checker.diagnostics.count == 1)
    }
  }

  func testDownCast() {

    // let a: Any = ()
    // let _ = a as! ()

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "a")))))),
        annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Any")))))),
      initializer: AnyExprID(ast.insert(TupleExpr()))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(CastExpr(
        left: AnyExprID(ast.insert(NameExpr(
          name: SourceRepresentable(value: "a")))),
        right: AnyTypeExprID(ast.insert(TupleTypeExpr())),
        direction: .down)))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testUpcast() {

    // let _ = () as Any

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(CastExpr(
        left: AnyExprID(ast.insert(TupleExpr())),
        right: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Any")))),
        direction: .up)))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testClosureTypeInference() {

    // let _ = fun () { 42 }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(LambdaExpr(decl: ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        body: SourceRepresentable(
          value: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "42"))))),
        isInExprContext: true)))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testClosureTypeInferenceWithHints() {

    // let _ = fun (x: sink Int) { x }
    // let _: thin (x: sink Int) -> Int = fun (x) { x }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(LambdaExpr(decl: ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        parameters: [
          ast.insert(ParameterDecl(
            label: SourceRepresentable(value: "x"),
            identifier: SourceRepresentable(value: "x"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .sink),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Int")))))))),
        ],
        body: SourceRepresentable(
          value: .expr(AnyExprID(ast.insert(NameExpr(
            name: SourceRepresentable(value: "x")))))),
        isInExprContext: true)))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())),
        annotation: AnyTypeExprID(ast.insert(LambdaTypeExpr(
          environment: SourceRepresentable(
            value: AnyTypeExprID(ast.insert(TupleTypeExpr()))),
          parameters: [
            LambdaTypeExpr.Parameter(
              label: "x",
              type: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .sink),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Int"))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Int"))))))))),
      initializer: AnyExprID(ast.insert(LambdaExpr(
        decl: ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          parameters: [
            ast.insert(ParameterDecl(
              label: SourceRepresentable(value: "x"),
              identifier: SourceRepresentable(value: "x")))
          ],
          body: SourceRepresentable(value: .expr(AnyExprID(ast.insert(NameExpr(
            name: SourceRepresentable(value: "x")))))),
          isInExprContext: true)))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testComplexClosureType() {

    // let _ = fun (x: sink ()) -> () {
    //   let y = x
    //   return y
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(LambdaExpr(
        decl: ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          parameters: [
            ast.insert(ParameterDecl(
              label: SourceRepresentable(value: "x"),
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .sink),
                bareType: AnyTypeExprID(ast.insert(TupleTypeExpr()))
              )))),
          ],
          output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt(
              stmts: [
                AnyStmtID(ast.insert(DeclStmt(decl: AnyDeclID(ast.insert(BindingDecl(
                  pattern: ast.insert(BindingPattern(
                    introducer: SourceRepresentable(value: .let),
                    subpattern: AnyPatternID(ast.insert(NamePattern(
                      decl: ast.insert(VarDecl(
                        identifier: SourceRepresentable(value: "y")))))))),
                  initializer: AnyExprID(ast.insert(NameExpr(
                    name: SourceRepresentable(value: "x")))))))))),
                AnyStmtID(ast.insert(ReturnStmt(
                  value: AnyExprID(ast.insert(NameExpr(
                    name: SourceRepresentable(value: "y")))))))
              ])))),
          isInExprContext: true)))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testComplexClosureTypeInferenceFailure() {

    // let _ = fun (x: sink ())) {} // error

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(LambdaExpr(
        decl: ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          parameters: [
            ast.insert(ParameterDecl(
              label: SourceRepresentable(value: "x"),
              identifier: SourceRepresentable(value: "x"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .sink),
                bareType: AnyTypeExprID(ast.insert(TupleTypeExpr()))
              )))),
          ],
          body: SourceRepresentable(
            value: .block(ast.insert(BraceStmt()))),
          isInExprContext: true)))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testReturnStmt() {

    // fun forty_two() -> Int { return 42 } // OK
    // fun forty_one() -> Int { return }    // error

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "forty_two"),
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "Int")))),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(ReturnStmt(
              value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "42")))))),
          ]))))))))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "forty_one"),
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "Int")))),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(ReturnStmt())),
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
  }

  func testImplicitImmutableCapture() {

    // fun main() {
    //   let foo = ()
    //   fun local { let bar = foo }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "main"),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "foo")))))))),
                initializer: AnyExprID(ast.insert(TupleExpr())))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(FunDecl(
                introducer: SourceRepresentable(value: .fun),
                identifier: SourceRepresentable(value: "local"),
                body: SourceRepresentable(
                  value: .block(ast.insert(BraceStmt(
                    stmts: [
                      AnyStmtID(ast.insert(DeclStmt(
                        decl: AnyDeclID(ast.insert(BindingDecl(
                          pattern: ast.insert(BindingPattern(
                            introducer: SourceRepresentable(value: .let),
                            subpattern: AnyPatternID(ast.insert(NamePattern(
                              decl: ast.insert(VarDecl(
                                identifier: SourceRepresentable(value: "bar")))))))),
                          initializer: AnyExprID(ast.insert(NameExpr(
                            name: SourceRepresentable(value: "foo")))))))))),
                    ])))))))))),
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testImplicitMutableCapture() {

    // fun main() {
    //   var foo = ()
    //   fun local { foo = () }
    // }

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "main"),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .var),
                  subpattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "foo")))))))),
                initializer: AnyExprID(ast.insert(TupleExpr())))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(FunDecl(
                introducer: SourceRepresentable(value: .fun),
                identifier: SourceRepresentable(value: "local"),
                body: SourceRepresentable(
                  value: .block(ast.insert(BraceStmt(
                    stmts: [
                      AnyStmtID(ast.insert(ExprStmt(
                        expr: AnyExprID(ast.insert(AssignExpr(
                          left: AnyExprID(ast.insert(NameExpr(
                            name: SourceRepresentable(value: "foo")))),
                          right: AnyExprID(ast.insert(TupleExpr())))))))),
                    ])))))))))),
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testCondExpr() {

    // fun one_or_two(_ test: Bool) -> Int {
    //   let x = if test { 1 } else { 2 }
    //   return x
    // }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "one_or_two"),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "test"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Bool")))))))),
      ], output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "Int")))),
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(NamePattern(
                    decl: ast.insert(VarDecl(
                      identifier: SourceRepresentable(value: "x")))))))),
                initializer: AnyExprID(ast.insert(CondExpr(
                  condition: [
                    .expr(AnyExprID(ast.insert(BooleanLiteralExpr(value: true))))
                  ],
                  success: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "1")))),
                  failure: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "2")))))))
              )))))),
            AnyStmtID(ast.insert(ReturnStmt(
              value: AnyExprID(ast.insert(NameExpr(
                name: SourceRepresentable(value: "x")))))))
          ]))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testDuplicateOperatorDecl() {

    // prefix operator -                  // OK
    // infix operator - : Addition        // OK
    // infix operator + : Addition        // error
    // infix operator + : Multiplication  // error

    // Create a fake source ranges to get different diagnostic locations.
    let file = SourceFile(contents: "abcde")
    let locations = file.contents.indices.map({ SourceLocation(source: file, index: $0) })

    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    let d0 = ast.insert(OperatorDecl(
      notation: SourceRepresentable(value: .prefix),
      name: SourceRepresentable(value: "-")))
    ast.ranges[d0] = locations[0] ..< locations[1]
    ast[main].members.append(AnyDeclID(d0))

    let d1 = ast.insert(OperatorDecl(
      notation: SourceRepresentable(value: .infix),
      name: SourceRepresentable(value: "-"),
      precedenceGroup: SourceRepresentable(value: .addition)))
    ast.ranges[d1] = locations[1] ..< locations[2]
    ast[main].members.append(AnyDeclID(d1))

    let d2 = ast.insert(OperatorDecl(
      notation: SourceRepresentable(value: .infix),
      name: SourceRepresentable(value: "+"),
      precedenceGroup: SourceRepresentable(value: .addition)))
    ast.ranges[d2] = locations[2] ..< locations[3]
    ast[main].members.append(AnyDeclID(d2))

    let d3 = ast.insert(OperatorDecl(
      notation: SourceRepresentable(value: .infix),
      name: SourceRepresentable(value: "+"),
      precedenceGroup: SourceRepresentable(value: .multiplication)))
    ast.ranges[d3] = locations[3] ..< locations[4]
    ast[main].members.append(AnyDeclID(d3))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 2)
  }

}
