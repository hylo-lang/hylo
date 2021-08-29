import AST
import Basic

/// A node walker that applies a solution produced by a type solver to the AST.
struct TypeDispatcher: NodeWalker {

  typealias Result = Bool

  var parent: Node?

  var innermostSpace: DeclSpace?

  init(solution: Solution, freeVarBindingPolicy: FreeTypeVarBindingPolicy) {
    self.solution = solution
    self.freeVarBindingPolicy = freeVarBindingPolicy
  }

  /// The solution to apply.
  let solution: Solution

  /// The binding policy to adopt for substituting free type variables.
  let freeVarBindingPolicy: FreeTypeVarBindingPolicy

  mutating func visit(_ decl: ValueDecl) -> Bool {
    // FIXME: Should this be uncontextualized?
    decl.type = solution.reify(decl.type, freeVariablePolicy: freeVarBindingPolicy)
    return true
  }

  mutating func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
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
      expr.type = solution.reify(expr.type, freeVariablePolicy: freeVarBindingPolicy)
    }

    return (true, expr)
  }

  mutating func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    pattern.type = solution.reify(pattern.type, freeVariablePolicy: freeVarBindingPolicy)

    if let decl = (pattern as? NamedPattern)?.decl {
      if decl.state < .typeChecked {
        decl.type = pattern.type.uncontextualized
        decl.setState(.typeChecked)
      }
      assert(!decl.type.isUnresolved)
    }

    return (true, pattern)
  }

  private func dispatch(_ expr: OverloadedDeclRefExpr) -> Expr {
    expr.type = solution.reify(expr.type, freeVariablePolicy: freeVarBindingPolicy)

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
    expr.type = solution.reify(expr.type, freeVariablePolicy: freeVarBindingPolicy)

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

