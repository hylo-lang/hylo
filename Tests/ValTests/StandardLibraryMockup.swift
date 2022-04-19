import Compiler

/// Creates a mockup of the standard library into `ast` and returns its identifier.
@discardableResult
func insertStandardLibraryMockup(into ast: inout AST) -> NodeID<ModuleDecl> {
  precondition(ast.stdlib == nil)
  let stdlib = ast.insert(ModuleDecl(name: "Val"))

  // trait ExpressibleByIntegerLiteral { ... }
  ast[stdlib].members.append(AnyDeclID(ast.insert(TraitDecl(
    identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral"),
    members: [
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        parameters: [
          ast.insert(ParamDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Builtin")))),
              identifier: SourceRepresentable(value: "IntegerLiteral")))))),
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
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        accessModifier: SourceRepresentable(value: .public),
        parameters: [
          ast.insert(ParamDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Builtin")))),
              identifier: SourceRepresentable(value: "IntegerLiteral")))))),
        ],
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "fatal_error"))))))))))))
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
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .`init`),
        accessModifier: SourceRepresentable(value: .public),
        parameters: [
          ast.insert(ParamDecl(
            identifier: SourceRepresentable(value: "integer_literal"),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              domain: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Builtin")))),
              identifier: SourceRepresentable(value: "IntegerLiteral")))))),
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
