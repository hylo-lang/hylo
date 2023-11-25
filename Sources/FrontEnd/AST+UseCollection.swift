import Core
import Utils

extension AST {

  /// Returns a map from used name to its mutability.
  ///
  /// This method collects all name expressions that occurs in `d`, visiting its children in
  /// pre-order. Nested type and extension declarations are not visited.
  func uses(in d: AnyDeclID) -> [(NameExpr.ID, AccessEffect)] {
    var v = UseVisitor()
    walk(d, notifying: &v)
    return v.uses
  }

  /// Returns a map from used name to its mutability.
  ///
  /// This method collects all name expressions that occurs in `e`, visiting its children in
  /// pre-order. Nested type and extension declarations are not visited.
  func uses(in e: AnyExprID) -> [(NameExpr.ID, AccessEffect)] {
    var v = UseVisitor()
    walk(e, notifying: &v)
    return v.uses
  }

}

/// The state of the visitor gathering uses.
private struct UseVisitor: ASTWalkObserver {

  /// The names being used with their visibility.
  private(set) var uses: [(NameExpr.ID, AccessEffect)] = []

  /// Records a use of `n` that with mutability `k`.
  private mutating func recordOccurrence(_ k: AccessEffect, _ n: NameExpr.ID) {
    uses.append((n, k))
  }

  /// Returns the name at the root of the given `lvalue`.
  private func root(_ lvalue: AnyExprID, in ast: AST) -> NameExpr.ID? {
    switch lvalue.kind {
    case NameExpr.self:
      return NameExpr.ID(lvalue)!
    case SubscriptCallExpr.self:
      return root(ast[SubscriptCallExpr.ID(lvalue)!].callee, in: ast)
    default:
      return nil
    }
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    switch n.kind {
    case InoutExpr.self:
      return visit(inoutExpr: .init(n)!, in: ast)
    case NameExpr.self:
      return visit(nameExpr: .init(n)!, in: ast)
    case ProductTypeDecl.self, TraitDecl.self, TypeAliasDecl.self:
      return false
    case ConformanceDecl.self, ExtensionDecl.self:
      return false
    default:
      return true
    }
  }

  private mutating func visit(inoutExpr e: InoutExpr.ID, in ast: AST) -> Bool {
    if let x = root(ast[e].subject, in: ast) {
      recordOccurrence(.inout, x)
      return false
    } else {
      return true
    }
  }

  private mutating func visit(nameExpr e: NameExpr.ID, in ast: AST) -> Bool {
    if ast[e].domain == .none {
      recordOccurrence(.let, e)
      return false
    } else {
      return true
    }
  }

}
