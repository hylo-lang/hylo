import Compiler

/// Creates a mockup of the standard library into `ast` and returns its identifier.
@discardableResult
func insertStandardLibraryMockup(into ast: inout AST) -> NodeID<ModuleDecl> {
  precondition(ast.stdlib == nil)
  let stdlib = ast.insert(ModuleDecl(name: "Val"))

  // fun fatal_error() -> Never {}
  ast[stdlib].members.append(AnyDeclID(ast.insert(FunDecl(
    introducer: SourceRepresentable(value: .fun),
    identifier: SourceRepresentable(value: "fatal_error"),
    output: AnyTypeExprID(ast.insert(NameTypeExpr(
      identifier: SourceRepresentable(value: "Never")))),
    body: SourceRepresentable(value: .block(ast.insert(BraceStmt())))))))

  // trait ExpressibleByIntegerLiteral { ... }
  ast[stdlib].members.append(AnyDeclID(ast.insert(TraitDecl(
    identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral"),
    members: [
      // init(integer_literal: Builtin.IntegerLiteral)
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Builtin")))),
                identifier: SourceRepresentable(value: "IntegerLiteral")))))))),
        ])))
    ]
  ))))

  // type Int { ... }
  ast[stdlib].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
    identifier: SourceRepresentable(value: "Int"),
    conformances: [
      ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral"))),
    ],
    members: [
      // public init(integer_literal: Builtin.IntegerLiteral) { fatal_error() }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        accessModifier: SourceRepresentable(value: .public),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Builtin")))),
                identifier: SourceRepresentable(value: "IntegerLiteral")))))))),
        ],
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "fatal_error")))))))))))),

      // public fun copy() -> Self { fatal_error() }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        identifier: SourceRepresentable(value: "copy"),
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "fatal_error")))))))))))),
    ]
  ))))

  // type Double { ... }
  ast[stdlib].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
    identifier: SourceRepresentable(value: "Double"),
    conformances: [
      ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral"))),
    ],
    members: [
      // public init(integer_literal: Builtin.IntegerLiteral) { fatal_error() }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        accessModifier: SourceRepresentable(value: .public),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Builtin")))),
                identifier: SourceRepresentable(value: "IntegerLiteral")))))))),
        ],
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "fatal_error"))))))))))))
    ]
  ))))

  ast.stdlib = stdlib
  return stdlib
}
