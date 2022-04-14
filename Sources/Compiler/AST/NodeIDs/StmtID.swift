import Utils

/// The ID of a statement.
public protocol StmtID: NodeIDProtocol {}

extension NodeID: StmtID where T: Stmt {}

/// The type-erased ID of a statement.
public struct AnyStmtID: StmtID {

  /// The underlying type-erased ID.
  var base: AnyNodeID

  /// Creates a type-erased ID from a statement ID.
  public init<T: StmtID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case BraceStmt.kind:
      return visitor.visit(brace: NodeID(rawValue: base.rawValue))
    case BreakStmt.kind:
      return visitor.visit(break: NodeID(rawValue: base.rawValue))
    case ContinueStmt.kind:
      return visitor.visit(continue: NodeID(rawValue: base.rawValue))
    case DeclStmt.kind:
      return visitor.visit(decl: NodeID(rawValue: base.rawValue))
    case DoWhileStmt.kind:
      return visitor.visit(doWhile: NodeID(rawValue: base.rawValue))
    case ExprStmt.kind:
      return visitor.visit(expr: NodeID(rawValue: base.rawValue))
    case ForStmt.kind:
      return visitor.visit(for: NodeID(rawValue: base.rawValue))
    case ReturnStmt.kind:
      return visitor.visit(return: NodeID(rawValue: base.rawValue))
    case WhileStmt.kind:
      return visitor.visit(while: NodeID(rawValue: base.rawValue))
    case YieldStmt.kind:
      return visitor.visit(yield: NodeID(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
