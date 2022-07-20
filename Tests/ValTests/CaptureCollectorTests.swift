import XCTest
@testable import Compiler

final class CaptureCollectorTests: XCTestCase {

  func testFunctionBindings() {

    // fun f<X, v: size>[let c = ()](_ p: Any) {
    //   let _ = free   // captured
    //   let _ = X      // bound
    //   let _ = v      // bound
    //   let _ = c      // bound
    //   let _ = p      // bound
    // }

    var ast = AST()
    let fun = ast.insert(FunDecl(
      introducer: SourceRepresentable(value: .fun),
      identifier: SourceRepresentable(value: "f"),
      genericClause: SourceRepresentable(value: GenericClause(
        parameters: [
          .type(ast.insert(GenericTypeParamDecl(
            identifier: SourceRepresentable(value: "X")))),
          .size(ast.insert(GenericSizeParamDecl(
            identifier: SourceRepresentable(value: "v")))),
        ])),
      captures: [
        ast.insert(BindingDecl(
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .let),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "c")))))))),
          initializer: AnyExprID(ast.insert(TupleExpr())))),
      ],
      parameters: [
        ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "p"),
          annotation: ast.insert(ParameterTypeExpr(
            convention: SourceRepresentable(value: .let),
            bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Any")))))))),
      ],
      body: SourceRepresentable(
        value: .block(ast.insert(BraceStmt(
          stmts: [
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "free")))))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "X")))))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "v")))))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "c")))))))))),
            AnyStmtID(ast.insert(DeclStmt(
              decl: AnyDeclID(ast.insert(BindingDecl(
                pattern: ast.insert(BindingPattern(
                  introducer: SourceRepresentable(value: .let),
                  subpattern: AnyPatternID(ast.insert(WildcardPattern())))),
                initializer: AnyExprID(ast.insert(NameExpr(
                  name: SourceRepresentable(value: "p")))))))))),
          ]))))))

    var collector = CaptureCollector(ast: ast)
    let captures = collector.freeNames(in: fun)
    XCTAssertEqual(captures.count, 2)
    XCTAssert(captures.keys.contains(Name(stem: "free")))
    XCTAssert(captures.keys.contains(Name(stem: "c")))
  }

}
