import AST
import Basic

/// A node walker that applies a solution produced by a type solver to the AST.
final class TypeDispatcher: NodeWalker {

  init(
    solution: Solution,
    freeVariablePolicy: FreeTypeVarBindingPolicy = .bindToErrorType
  ) {
    self.solution = solution
    self.freeVariablePolicy = freeVariablePolicy
  }

  /// The solution to apply.
  let solution: Solution

  /// The binding policy to adopt for free type variables.
  let freeVariablePolicy: FreeTypeVarBindingPolicy

  override func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    if let valueDecl = decl as? ValueDecl {
      valueDecl.type = solution.reify(valueDecl.type, freeVariablePolicy: freeVariablePolicy)
    }
    return (true, decl)
  }

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    switch expr {
    case is MemberRefExpr:
      break

    case let e as UnresolvedMemberExpr:
      return (true, dispatch(e))

    case let e as OverloadedDeclRefExpr:
      return (true, dispatch(e))

    case is UnresolvedDeclRefExpr, is UnresolvedQualDeclRefExpr:
      fatalError("unexpected primary unresolved expr")

    default:
      expr.type = solution.reify(expr.type, freeVariablePolicy: freeVariablePolicy)
    }

    return (true, expr)
  }

  override func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    switch pattern {
    case is NamedPattern:
      // Nothing to do here.
      break

    default:
      pattern.type = solution.reify(pattern.type, freeVariablePolicy: freeVariablePolicy)
    }

    return (true, pattern)
  }

  private func dispatch(_ expr: OverloadedDeclRefExpr) -> Expr {
    expr.type = solution.reify(expr.type, freeVariablePolicy: freeVariablePolicy)

    // Retrieve the selected overload from the solution.
    let locator = ConstraintLocator(expr)
    let selected = solution.overloadChoices[locator] ?? expr.declSet

    if selected.count == 1 {
      return DeclRefExpr(decl: selected[0], type: expr.type, range: expr.range)
    }

    expr.declSet = selected
    return expr
  }

  private func dispatch(_ expr: UnresolvedMemberExpr) -> Expr {
    expr.type = solution.reify(expr.type, freeVariablePolicy: freeVariablePolicy)

    // Retrieve the selected overload from the solution.
    let locator = ConstraintLocator(expr)
    if let selected = solution.overloadChoices[locator], selected.count == 1{
      return MemberRefExpr(base: expr.base, decl: selected[0], type: expr.type, range: expr.range)
    }

    return expr
  }

}

