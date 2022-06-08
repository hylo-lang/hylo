import Utils

/// The ID of a statement.
public protocol StmtID: NodeIDProtocol {}

extension StmtID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .braceStmt:
      return visitor.visit(brace: NodeID(unsafeRawValue: rawValue))
    case .breakStmt:
      return visitor.visit(break: NodeID(unsafeRawValue: rawValue))
    case .continueStmt:
      return visitor.visit(continue: NodeID(unsafeRawValue: rawValue))
    case .declStmt:
      return visitor.visit(decl: NodeID(unsafeRawValue: rawValue))
    case .discardStmt:
      return visitor.visit(discard: NodeID(unsafeRawValue: rawValue))
    case .doWhileStmt:
      return visitor.visit(doWhile: NodeID(unsafeRawValue: rawValue))
    case .exprStmt:
      return visitor.visit(expr: NodeID(unsafeRawValue: rawValue))
    case .forStmt:
      return visitor.visit(for: NodeID(unsafeRawValue: rawValue))
    case .returnStmt:
      return visitor.visit(return: NodeID(unsafeRawValue: rawValue))
    case .whileStmt:
      return visitor.visit(while: NodeID(unsafeRawValue: rawValue))
    case .yieldStmt:
      return visitor.visit(yield: NodeID(unsafeRawValue: rawValue))
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
