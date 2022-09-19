import Utils

/// The ID of a statement.
public protocol StmtID: NodeIDProtocol {}

extension StmtID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .braceStmt:
      return visitor.visit(brace: NodeID(rawValue: rawValue))
    case .breakStmt:
      return visitor.visit(break: NodeID(rawValue: rawValue))
    case .condBindingStmt:
      return visitor.visit(condBinding: NodeID(rawValue: rawValue))
    case .continueStmt:
      return visitor.visit(continue: NodeID(rawValue: rawValue))
    case .declStmt:
      return visitor.visit(decl: NodeID(rawValue: rawValue))
    case .discardStmt:
      return visitor.visit(discard: NodeID(rawValue: rawValue))
    case .doWhileStmt:
      return visitor.visit(doWhile: NodeID(rawValue: rawValue))
    case .exprStmt:
      return visitor.visit(expr: NodeID(rawValue: rawValue))
    case .forStmt:
      return visitor.visit(for: NodeID(rawValue: rawValue))
    case .returnStmt:
      return visitor.visit(return: NodeID(rawValue: rawValue))
    case .whileStmt:
      return visitor.visit(while: NodeID(rawValue: rawValue))
    case .yieldStmt:
      return visitor.visit(yield: NodeID(rawValue: rawValue))
    default:
      unreachable()
    }
  }

}

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

}
