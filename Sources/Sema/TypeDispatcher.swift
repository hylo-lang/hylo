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
    case is MemberRefExpr:
      break

    case let e as UnresolvedMemberExpr:
      return (true, dispatch(e))

    case let e as OverloadedDeclRefExpr:
      return (true, dispatch(e))

    case is UnresolvedDeclRefExpr, is UnresolvedQualDeclRefExpr:
      fatalError("unexpected primary unresolved expr")

    default:
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
    let decls = expr.declSet.filter({ decl in match(decl, type) })

    // Diagnose an ambigous name reference if there's not exactly one possible candidate.
    guard decls.count == 1 else {
      type.context.report(.ambiguousReference(to: expr.declSet[0].name, range: expr.range))
      return expr
    }

    // Substitute the overload set for a concrete declaration ref.
    let instType = decls[0].instantiate(from: innermostSpace!)
    return DeclRefExpr(decl: decls[0], type: instType, range: expr.range)
  }

  private func dispatch(_ expr: UnresolvedMemberExpr) -> Expr {
    let type = expr.type.accept(reifier)

    // The base expression should have a nominal type.
    let baseType: NominalType
    switch expr.base.type {
    case let nominalType as NominalType:
      baseType = nominalType
    case let inoutType as InoutType where inoutType.base is NominalType:
      baseType = inoutType.base as! NominalType
    default:
      type.context.report(
        .cannotFind(member: expr.memberName, in: expr.base.type, range: expr.range))
      return expr
    }

    // Search for the declaration that matches the expression's type.
    let decls = baseType.decl
      .lookup(unqualified: expr.memberName, in: type.context)
      .values
      .filter({ decl in match(decl, type) })

    guard !decls.isEmpty else {
      type.context.report(
        .cannotFind(member: expr.memberName, in: baseType, range: expr.base.range))
      return expr
    }

    // Diagnose an ambigous name reference if there's not exactly one candidate left.
    guard decls.count == 1 else {
      type.context.report(.ambiguousReference(to: decls[0].name, range: expr.range))
      return expr
    }

    let instType = decls[0].instantiate(from: innermostSpace!)
    return MemberRefExpr(base: expr.base, decl: decls[0], type: instType, range: expr.range)
  }

  private func match(_ decl: TypeOrValueDecl, _ type: ValType) -> Bool {
    let declType = decl.type.accept(reifier)
    assert(!(declType is UnresolvedType))

    if let inoutType = declType as? InoutType {
     return inoutType.base == type
    }
    return declType == type
  }

}
