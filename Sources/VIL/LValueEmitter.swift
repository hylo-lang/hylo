import AST

/// A visitor that emits the VIL code of an l-value.
struct LValueEmitter: ExprVisitor {

  typealias ExprResult = Value

  /// The function emitter that owns this emitter.
  unowned let parent: FunctionEmitter

  func visit(_ node: IntLiteralExpr) -> Value {
    fatalError()
  }

  func visit(_ node: AssignExpr) -> Value {
    fatalError()
  }

  func visit(_ node: TupleExpr) -> Value {
    fatalError()
  }

  func visit(_ node: CallExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedMemberExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: OverloadedDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: DeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: TypeDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: MemberRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: AddrOfExpr) -> Value {
    fatalError()
  }

  func visit(_ node: WildcardExpr) -> Value {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> Value {
    fatalError()
  }

}
