import AST
import Basic

final class TypeDispatcher: NodeWalker {

  init(reifier: TypeReifier) {
    self.reifier = reifier
  }

  let reifier: TypeReifier

  override func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    if let valueDecl = decl as? ValueDecl {
      valueDecl.type = valueDecl.type.accept(reifier)
    }
    return (true, decl)
  }

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    switch expr {
    case is DeclRefExpr:
      // Nothing to do here.
      break

    case let e as OverloadedDeclRefExpr:
      return (true, dispatch(e))

    case let e as UnresolvedMemberExpr:
      return (true, dispatch(e))

    default:
      // TODO: Substitute `UnresolvedMemberExpr`.

      // Note that we don't need to emit any diagnostic for unresolved declaration refs, as those
      // are already diagnosed during name binding.
      expr.type = expr.type.accept(reifier)
    }

    return (true, expr)
  }

  override func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    switch pattern {
    case is NamedPattern:
      // Nothing to do here.
      break

    default:
      pattern.type = pattern.type.accept(reifier)
    }

    return (true, pattern)
  }

  private func dispatch(_ expr: OverloadedDeclRefExpr) -> Expr {
    let type = expr.type.accept(reifier)

    // Search for the declaration that matches the expression's type.
    let decls = expr.declSet.filter({ decl in decl.type.accept(reifier) === type })

    // Diagnose an ambigous name reference if there's not exactly one possible candidate.
    guard decls.count == 1 else {
      type.context.report(
        Diagnostic("ambiguous reference to '\(expr.declSet[0].name)'")
          .set(\.reportLocation, value: expr.range.lowerBound)
          .set(\.ranges, value: [expr.range]))
      return expr
    }

    // Substitute the overload set for a concrete declaration ref.
    return DeclRefExpr(decl: decls[0], range: expr.range)
  }

  private func dispatch(_ expr: UnresolvedMemberExpr) -> Expr {
    let type = expr.type.accept(reifier)

    // The base expression should have a nominal type.
    guard let typeDecl = (expr.base.type as? NominalType)?.decl else {
      type.context.report(
        .cannotFind(name: expr.memberName, in: expr.base.type, range: expr.range))
      return expr
    }

    // Search for the declaration that matches the expression's type.
    let decls = typeDecl
      .lookup(expr.memberName, in: type.context)
      .valueDecls
      .filter({ decl in decl.type.accept(reifier) === type })

    guard !decls.isEmpty else {
      type.context.report(
        Diagnostic("value of type '\(expr.base.type)' has no member '\(expr.memberName)'")
          .set(\.reportLocation, value: expr.base.range.lowerBound)
          .set(\.ranges, value: [expr.base.range]))
      return expr
    }

    // Diagnose an ambigous name reference if there's more than one possible candidate.
    guard decls.count == 1 else {
      type.context.report(
        Diagnostic("ambiguous reference to '\(decls[0].name)'")
          .set(\.reportLocation, value: expr.range.lowerBound)
          .set(\.ranges, value: [expr.range]))
      return expr
    }

    return MemberRefExpr(base: expr.base, decl: decls[0], range: expr.range)
  }

}
