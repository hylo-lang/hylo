import Compiler
import Library

func main() {
  var ast = AST()
  let main = ast.insert(ModuleDecl(name: "Val"))
  ast.stdlib = main

  // fun factorial(_ n: Int) -> Int {
  //   if n < 2 { 0 } else { n * factorial(n - 1) }
  // }
  ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
    introducer: SourceRepresentable(value: .fun),
    identifier: SourceRepresentable(value: "factorial"),
    parameters: [
      ast.insert(ParameterDecl(
        identifier: SourceRepresentable(value: "n"),
        annotation: ast.insert(ParameterTypeExpr(
          convention: SourceRepresentable(value: .let),
          bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Int")))))))),
    ],
    output: AnyTypeExprID(ast.insert(NameTypeExpr(
      identifier: SourceRepresentable(value: "Int")))),
    body: SourceRepresentable(value: .expr(
      AnyExprID(ast.insert(CondExpr(
        condition: [
          .expr(AnyExprID(ast.insert(SequenceExpr.unfolded([
            AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "n")))),
            AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "<"),
              notation: .infix))),
            AnyExprID(ast.insert(IntegerLiteralExpr(value: "2"))),
          ]))))
        ],
        success: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "1")))),
        failure: .expr(AnyExprID(ast.insert(SequenceExpr.unfolded([
          AnyExprID(ast.insert(NameExpr(
            stem: SourceRepresentable(value: "n")))),
          AnyExprID(ast.insert(NameExpr(
            stem: SourceRepresentable(value: "*"),
            notation: .infix))),
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "factorial")))),
            arguments: [
              SourceRepresentable(
                value: CallArgument(
                  value: AnyExprID(ast.insert(SequenceExpr.unfolded([
                    AnyExprID(ast.insert(NameExpr(
                      stem: SourceRepresentable(value: "n")))),
                    AnyExprID(ast.insert(NameExpr(
                      stem: SourceRepresentable(value: "-"),
                      notation: .infix))),
                    AnyExprID(ast.insert(IntegerLiteralExpr(value: "1"))),
                  ]))))),
            ]))),
        ])))))))))))))

  // public fun main() {
  //   let x = factoria(6)
  //   Builtin.i64_print(x.value)
  // }
  ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
    introducer: SourceRepresentable(value: .fun),
    accessModifier: SourceRepresentable(value: .public),
    identifier: SourceRepresentable(value: "main"),
    body: SourceRepresentable(value: .block(ast.insert(BraceStmt(
      stmts: [
        AnyStmtID(ast.insert(DeclStmt(
          decl: AnyDeclID(ast.insert(BindingDecl(
            pattern: ast.insert(BindingPattern(
              introducer: SourceRepresentable(value: .let),
              subpattern: AnyPatternID(ast.insert(NamePattern(
                decl: ast.insert(VarDecl(
                  identifier: SourceRepresentable(value: "x")))))))),
            initializer: AnyExprID(ast.insert(FunCallExpr(
              callee: AnyExprID(ast.insert(NameExpr(
                stem: SourceRepresentable(value: "factorial")))),
              arguments: [
                SourceRepresentable(
                  value: CallArgument(
                    value: AnyExprID(ast.insert(IntegerLiteralExpr(value: "6"))))),
              ]))))))))),
        AnyStmtID(ast.insert(ExprStmt(
          expr: AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              domain: .explicit(AnyExprID(ast.insert(NameExpr(
                stem: SourceRepresentable(value: "Builtin"))))),
              stem: SourceRepresentable(value: "i64_print")))),
          arguments: [
            SourceRepresentable(
              value: CallArgument(
                value: AnyExprID(ast.insert(NameExpr(
                  domain: .explicit(AnyExprID(ast.insert(NameExpr(
                    stem: SourceRepresentable(value: "x"))))),
                  stem: SourceRepresentable(value: "value")))))),
          ])))))),
      ]))))))))

  // infix operator * : Multiplication
  ast[main].members.append(AnyDeclID(ast.insert(OperatorDecl(
    notation: SourceRepresentable(value: .infix),
    name: SourceRepresentable(value: "*"),
    precedenceGroup: SourceRepresentable(value: .multiplication)))))

  // infix operator + : Addition
  ast[main].members.append(AnyDeclID(ast.insert(OperatorDecl(
    notation: SourceRepresentable(value: .infix),
    name: SourceRepresentable(value: "+"),
    precedenceGroup: SourceRepresentable(value: .addition)))))

  // infix operator - : Addition
  ast[main].members.append(AnyDeclID(ast.insert(OperatorDecl(
    notation: SourceRepresentable(value: .infix),
    name: SourceRepresentable(value: "-"),
    precedenceGroup: SourceRepresentable(value: .addition)))))

  // infix operator < : Comparison
  ast[main].members.append(AnyDeclID(ast.insert(OperatorDecl(
    notation: SourceRepresentable(value: .infix),
    name: SourceRepresentable(value: "<"),
    precedenceGroup: SourceRepresentable(value: .comparison)))))

  // public fun fatal_error() -> Never { Builtin.terminate() }
  ast[main].members.append(AnyDeclID(ast.insert(FunDecl(
    introducer: SourceRepresentable(value: .fun),
    accessModifier: SourceRepresentable(value: .public),
    identifier: SourceRepresentable(value: "fatal_error"),
    output: AnyTypeExprID(ast.insert(NameTypeExpr(
      identifier: SourceRepresentable(value: "Never")))),
    body: SourceRepresentable(value: .expr(
      AnyExprID(ast.insert(FunCallExpr(
        callee: AnyExprID(ast.insert(NameExpr(
          domain: .explicit(AnyExprID(ast.insert(NameExpr(
            stem: SourceRepresentable(value: "Builtin"))))),
          stem: SourceRepresentable(value: "terminate")))))))))))))

  // public type Bool { ... }
  ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
    accessModifier: SourceRepresentable(value: .public),
    identifier: SourceRepresentable(value: "Bool"),
    members: [
      // var value: Builtin.i1
      AnyDeclID(ast.insert(BindingDecl(
        pattern: ast.insert(BindingPattern(
          introducer: SourceRepresentable(value: .var),
          subpattern: AnyPatternID(ast.insert(NamePattern(
            decl: ast.insert(VarDecl(
              identifier: SourceRepresentable(value: "value")))))),
          annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
            domain: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Builtin")))),
            identifier: SourceRepresentable(value: "i1"))))))))),

      // public fun copy() -> Self {
      //   Bool(value: Builtin.i1_copy(value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        identifier: SourceRepresentable(value: "copy"),
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Bool")))),
          arguments: [
            SourceRepresentable(
              value: CallArgument(
                label: SourceRepresentable(value: "value"),
                value: AnyExprID(ast.insert(FunCallExpr(
                  callee: AnyExprID(ast.insert(NameExpr(
                    domain: .explicit(AnyExprID(ast.insert(NameExpr(
                      stem: SourceRepresentable(value: "Builtin"))))),
                    stem: SourceRepresentable(value: "i1_copy")))),
                arguments: [
                  SourceRepresentable(
                    value: CallArgument(
                      value: AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "value")))))),
                ]))))),
          ])))))))),
    ]))))

  // trait ExpressibleByIntegerLiteral { ... }
  ast[main].members.append(AnyDeclID(ast.insert(TraitDecl(
    identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral")))))

  // public type Int { ... }
  ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
    accessModifier: SourceRepresentable(value: .public),
    identifier: SourceRepresentable(value: "Int"),
    conformances: [
      ast.insert(NameTypeExpr(
        identifier: SourceRepresentable(value: "ExpressibleByIntegerLiteral"))),
    ],
    members: [
      // var value: Builtin.i64
      AnyDeclID(ast.insert(BindingDecl(
        pattern: ast.insert(BindingPattern(
          introducer: SourceRepresentable(value: .var),
          subpattern: AnyPatternID(ast.insert(NamePattern(
            decl: ast.insert(VarDecl(
              identifier: SourceRepresentable(value: "value")))))),
          annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
            domain: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Builtin")))),
            identifier: SourceRepresentable(value: "i64"))))))))),

      // public infix fun * (_ other: Self) -> Self {
      //   Int(value: Builtin.i64_mul(value, other.value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        notation: SourceRepresentable(value: .infix),
        identifier: SourceRepresentable(value: "*"),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "other"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Self")))))))),
        ],
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Int")))),
            arguments: [
              SourceRepresentable(
                value: CallArgument(
                  label: SourceRepresentable(value: "value"),
                  value: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      domain: .explicit(AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "Builtin"))))),
                      stem: SourceRepresentable(value: "i64_mul")))),
                  arguments: [
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: "value")))))),
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          domain: .explicit(AnyExprID(ast.insert(NameExpr(
                            stem: SourceRepresentable(value: "other"))))),
                          stem: SourceRepresentable(value: "value")))))),
                  ]))))),
            ])))))))),

      // public infix fun + (_ other: Self) -> Self {
      //   Int(value: Builtin.i64_add(value, other.value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        notation: SourceRepresentable(value: .infix),
        identifier: SourceRepresentable(value: "+"),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "other"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Self")))))))),
        ],
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Int")))),
            arguments: [
              SourceRepresentable(
                value: CallArgument(
                  label: SourceRepresentable(value: "value"),
                  value: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      domain: .explicit(AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "Builtin"))))),
                      stem: SourceRepresentable(value: "i64_add")))),
                  arguments: [
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: "value")))))),
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          domain: .explicit(AnyExprID(ast.insert(NameExpr(
                            stem: SourceRepresentable(value: "other"))))),
                          stem: SourceRepresentable(value: "value")))))),
                  ]))))),
            ])))))))),

      // public infix fun - (_ other: Self) -> Self {
      //   Int(value: Builtin.i64_add(value, other.value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        notation: SourceRepresentable(value: .infix),
        identifier: SourceRepresentable(value: "-"),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "other"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Self")))))))),
        ],
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Int")))),
            arguments: [
              SourceRepresentable(
                value: CallArgument(
                  label: SourceRepresentable(value: "value"),
                  value: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      domain: .explicit(AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "Builtin"))))),
                      stem: SourceRepresentable(value: "i64_sub")))),
                  arguments: [
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: "value")))))),
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          domain: .explicit(AnyExprID(ast.insert(NameExpr(
                            stem: SourceRepresentable(value: "other"))))),
                          stem: SourceRepresentable(value: "value")))))),
                  ]))))),
            ])))))))),

      // public infix fun < (_ other: Self) -> Bool {
      //   Bool(value: Builtin.i64_lt(value, other.value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        notation: SourceRepresentable(value: .infix),
        identifier: SourceRepresentable(value: "<"),
        parameters: [
          ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "other"),
            annotation: ast.insert(ParameterTypeExpr(
              convention: SourceRepresentable(value: .let),
              bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                identifier: SourceRepresentable(value: "Self")))))))),
        ],
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Bool")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Bool")))),
            arguments: [
              SourceRepresentable(
                value: CallArgument(
                  label: SourceRepresentable(value: "value"),
                  value: AnyExprID(ast.insert(FunCallExpr(
                    callee: AnyExprID(ast.insert(NameExpr(
                      domain: .explicit(AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "Builtin"))))),
                      stem: SourceRepresentable(value: "i64_lt")))),
                  arguments: [
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          stem: SourceRepresentable(value: "value")))))),
                    SourceRepresentable(
                      value: CallArgument(
                        value: AnyExprID(ast.insert(NameExpr(
                          domain: .explicit(AnyExprID(ast.insert(NameExpr(
                            stem: SourceRepresentable(value: "other"))))),
                          stem: SourceRepresentable(value: "value")))))),
                  ]))))),
            ])))))))),

      // public fun copy() -> Self {
      //   Int(value: Builtin.i64_copy(value))
      // }
      AnyDeclID(ast.insert(FunDecl(
        introducer: SourceRepresentable(value: .fun),
        accessModifier: SourceRepresentable(value: .public),
        identifier: SourceRepresentable(value: "copy"),
        output: AnyTypeExprID(ast.insert(NameTypeExpr(
          identifier: SourceRepresentable(value: "Self")))),
        body: SourceRepresentable(value: .expr(
          AnyExprID(ast.insert(FunCallExpr(
            callee: AnyExprID(ast.insert(NameExpr(
              stem: SourceRepresentable(value: "Int")))),
          arguments: [
            SourceRepresentable(
              value: CallArgument(
                label: SourceRepresentable(value: "value"),
                value: AnyExprID(ast.insert(FunCallExpr(
                  callee: AnyExprID(ast.insert(NameExpr(
                    domain: .explicit(AnyExprID(ast.insert(NameExpr(
                      stem: SourceRepresentable(value: "Builtin"))))),
                    stem: SourceRepresentable(value: "i64_copy")))),
                arguments: [
                  SourceRepresentable(
                    value: CallArgument(
                      value: AnyExprID(ast.insert(NameExpr(
                        stem: SourceRepresentable(value: "value")))))),
                ]))))),
          ])))))))),
    ]
  ))))

  var checker = TypeChecker(ast: ast)
  checker.isProcessingStandardLibrary = true
  guard let typedProgram = checker.run() else {
    print(checker.diagnostics)
    return
  }

  var emitter = Emitter(program: typedProgram)
  let module = emitter.emit(module: main)
  print(module)
}

main()
