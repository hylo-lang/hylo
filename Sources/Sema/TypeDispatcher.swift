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
      // FIXME: Should this be uncontextualized?
      valueDecl.type = solution.reify(valueDecl.type, freeVariablePolicy: freeVariablePolicy)
    }
    return (true, decl)
  }

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    switch expr {
    case is MemberDeclRefExpr:
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

  override func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    pattern.type = solution.reify(pattern.type, freeVariablePolicy: freeVariablePolicy)

    if let decl = (pattern as? NamedPattern)?.decl {
      if decl.state < .typeChecked {
        decl.type = pattern.type.uncontextualized
        decl.setState(.typeChecked)
      }
      assert(!(decl.type is UnresolvedType))
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

    // If the base has a tuple type, this refers to the first element labeled after the member.
    if let tupleType = expr.base.type as? TupleType {
      if let index = tupleType.elems.firstIndex(where: { $0.label == expr.memberName }) {
        return TupleMemberExpr(
          base: expr.base, memberIndex: index, type: expr.type, range: expr.range)
      }

      // The expression couldn't be resolved.
      return expr
    }

    // Retrieve the selected overload from the solution.
    let locator = ConstraintLocator(expr)
    if let selected = solution.overloadChoices[locator], selected.count == 1{
      return MemberDeclRefExpr(
        base: expr.base, decl: selected[0], type: expr.type, range: expr.range)
    }

    // The expression couldn't be resolved.
    return expr
  }

}

