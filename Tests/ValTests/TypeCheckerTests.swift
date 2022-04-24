import XCTest
import Compiler

final class TypeCheckerTests: XCTestCase {

  func testTraitDecl() {

    // trait T {
    //   type X: T where X: T,        // OK
    //                   X: X,        // error: rhs is not a trait
    //                   Any == Never // error: neither type is a parameter
    //   type Y where X == Y          // OK
    //   size n where n > 0           // OK
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
                    l: AnyTypeExprID(ast.insertTypeName("Any")),
                    r: AnyTypeExprID(ast.insertTypeName("Never"))))
              ]))))),
        AnyDeclID(ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(value: "Y"),
          whereClause: SourceRepresentable(
            value: WhereClause(
              constraints: [
                SourceRepresentable(
                  value: .equality(
                    l: AnyTypeExprID(ast.insertTypeName("X")),
                    r: AnyTypeExprID(ast.insertTypeName("Y"))))
              ]))))),
        AnyDeclID(ast.insert(AssociatedSizeDecl(
          identifier: SourceRepresentable(value: "n"),
          whereClause: SourceRepresentable(
            value: WhereClause(
              constraints: [
                SourceRepresentable(
                  value: .size(
                    AnyExprID(ast.insert(UnfoldedExpr(
                      subexpressions: [
                        AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: "n")))),
                        AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: ">"),
                          notation: .infix))),
                        AnyExprID(ast.insert(IntegerLiteralExpr(value: "10"))),
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
    var i = file.startIndex
    var j = file.index(after: i)

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T", range: i ..< j),
      refinements: [ast.insertTypeName("U")]))))
    (i, j) = (j, file.index(after: j))

    ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "U", range: i ..< j),
      refinements: [ast.insertTypeName("V")]))))
    (i, j) = (j, file.index(after: j))

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
                SourceRepresentable(
                  value: TupleTypeExpr.Element(
                    label: "a",
                    type: AnyTypeExprID(ast.insertTypeName("Any")))),
                SourceRepresentable(
                  value: TupleTypeExpr.Element(
                    label: "b",
                    type: AnyTypeExprID(ast.insertTypeName("Never")))),
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
                SourceRepresentable(
                  value: TupleTypeExpr.Element(
                    label: "a",
                    type: AnyTypeExprID(ast.insertTypeName("Any")))),
                SourceRepresentable(
                  value: TupleTypeExpr.Element(
                    label: "b",
                    type: AnyTypeExprID(ast.insertTypeName("Never")))),
              ])))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssertEqual(checker.diagnostics.count, 1)
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
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(TupleExpr())))),
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(TupleExpr())))),
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
            SourceRepresentable(
              value: TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Int")))))),
            SourceRepresentable(
              value: TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Double")))))),
          ]))))),
      initializer: AnyExprID(ast.insert(TupleExpr(
        elements: [
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "2"))))),
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "3"))))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(NamePattern(
          decl: ast.insert(VarDecl(
            identifier: SourceRepresentable(value: "x0")))))),
        annotation: AnyTypeExprID(ast.insert(TupleTypeExpr(
          elements: [
            SourceRepresentable(
              value: TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insert(WildcardTypeExpr())))),
            SourceRepresentable(
              value: TupleTypeExpr.Element(
                type: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Double")))))),
          ]))))),
      initializer: AnyExprID(ast.insert(TupleExpr(
        elements: [
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "2"))))),
          SourceRepresentable(
            value: TupleExpr.Element(
              value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "3"))))),
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
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
          annotation: AnyTypeExprID(ast.insert(TupleTypeExpr())))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "b"),
          annotation: AnyTypeExprID(ast.insert(TupleTypeExpr())),
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
          annotation: AnyTypeExprID(ast.insert(TupleTypeExpr())))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "a"),
          annotation: AnyTypeExprID(ast.insert(TupleTypeExpr())))),
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
        params: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "T")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "T")))))))))
      ],
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "T")))),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          stem: SourceRepresentable(value: "x"))))))))))

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
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          stem: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr()))))
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
        params: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "Y")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x"),
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "X"))))))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "y"),
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Y"))))))))),
      ],
      output: AnyTypeExprID(ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "X")))),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          stem: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "a0",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "b0",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "a1",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "b1",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
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
        params: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
        ])),
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x0"),
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "X"))))))))),
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "x1"),
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr(elements: [
              SourceRepresentable(value: TupleTypeExpr.Element(
                label: "b",
                type: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "X"))))))
            ])))))))),
      ],
      body: SourceRepresentable(value: .block(ast.insert(BraceStmt())))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr())))),
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "b",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
        ])))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "a",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr(
              elements: [
                SourceRepresentable(value: TupleExpr.Element(
                  label: "b",
                  value: AnyExprID(ast.insert(TupleExpr())))),
              ]))))),
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
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          stem: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr())))),
          SourceRepresentable(value: CallArgument(
            value: AnyExprID(ast.insert(TupleExpr())))),
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
          annotation: AnyTypeExprID(ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .sink),
            bareType: AnyTypeExprID(ast.insert(TupleTypeExpr())))))))
      ],
      output: AnyTypeExprID(ast.insert(TupleTypeExpr())),
      body: SourceRepresentable(
        value: .expr(AnyExprID(ast.insert(NameExpr(
          stem: SourceRepresentable(value: "x"))))))))))

    ast[main].members.append(AnyDeclID(ast.insert(BindingDecl(
      pattern: ast.insert(BindingPattern(
        introducer: SourceRepresentable(value: .let),
        subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
      initializer: AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "f")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            label: SourceRepresentable(value: "y"),
            value: AnyExprID(ast.insert(TupleExpr()))))
        ])))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertFalse(checker.check(module: main))
    XCTAssert(checker.diagnostics.count == 1)
  }

  func testIllegalMemberwiseCtorDecl() {

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

  func testMemberwiseCtorCall() {

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
        callee: AnyExprID(ast.insert(NameExpr(stem: SourceRepresentable(value: "A")))),
        arguments: [
          SourceRepresentable(value: CallArgument(
            label: SourceRepresentable(value: "foo"),
            value: AnyExprID(ast.insert(TupleExpr())))),
          SourceRepresentable(value: CallArgument(
            label: SourceRepresentable(value: "bar"),
            value: AnyExprID(ast.insert(TupleExpr())))),
        ])))))))

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
      genericClause: SourceRepresentable(
        value: GenericClause(
          params: [
            .type(ast.insert(GenericTypeParamDecl(
              identifier: SourceRepresentable(value: "X")))),
            .type(ast.insert(GenericTypeParamDecl(
              identifier: SourceRepresentable(value: "Y")))),
          ])),
      body: SourceRepresentable(
        value: .typeExpr(
          AnyTypeExprID(ast.insert(TupleTypeExpr(
            elements: [
              SourceRepresentable(
                value: TupleTypeExpr.Element(
                  type: AnyTypeExprID(ast.insertTypeName("X")))),
              SourceRepresentable(
                value: TupleTypeExpr.Element(
                  type: AnyTypeExprID(ast.insertTypeName("Y")))),
            ])))))))))

    ast[main].members.append(AnyDeclID(ast.insert(TypeAliasDecl(
      identifier: SourceRepresentable(value: "AnyPair"),
      body: SourceRepresentable(
        value: .typeExpr(
          AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Pair"),
            arguments: [
              .type(AnyTypeExprID(ast.insertTypeName("Any"))),
              .type(AnyTypeExprID(ast.insertTypeName("Any"))),
            ])))))))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))
  }

  func testIntegerLiteralExpr() {
    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "main"))

    // 42
    let expr = AnyExprID(ast.insert(IntegerLiteralExpr(value: "42")))

    // Infer the type of the literal without any contextual information.
    do {
      var checker = TypeChecker(ast: ast)
      let type = checker.infer(expr: expr, inScope: main)
      XCTAssertEqual(type, .int(in: ast))
    }

    // Infer the type of the literal assuming it's `Double` from the context.
    do {
      var checker = TypeChecker(ast: ast)
      let type = checker.infer(expr: expr, expectedType: .double(in: ast), inScope: main)
      XCTAssertEqual(type, .double(in: ast))
    }

    // Infer the type of the literal assuming its `()` from the context.
    do {
      var checker = TypeChecker(ast: ast)
      let type = checker.infer(expr: expr, expectedType: .unit, inScope: main)
      XCTAssertNil(type)
      XCTAssert(checker.diagnostics.count == 1)
    }
  }

}
