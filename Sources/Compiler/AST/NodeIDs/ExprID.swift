import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension ExprID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .assignExpr:
      return visitor.visit(assign: NodeID(rawValue: rawValue))
    case .asyncExpr:
      return visitor.visit(async: NodeID(rawValue: rawValue))
    case .awaitExpr:
      return visitor.visit(await: NodeID(rawValue: rawValue))
    case .booleanLiteralExpr:
      return visitor.visit(booleanLiteral: NodeID(rawValue: rawValue))
    case .castExpr:
      return visitor.visit(cast: NodeID(rawValue: rawValue))
    case .condExpr:
      return visitor.visit(cond: NodeID(rawValue: rawValue))
    case .floatLiteralExpr:
      return visitor.visit(floatLiteral: NodeID(rawValue: rawValue))
    case .funCallExpr:
      return visitor.visit(funCall: NodeID(rawValue: rawValue))
    case .inoutExpr:
      return visitor.visit(`inout`: NodeID(rawValue: rawValue))
    case .integerLiteralExpr:
      return visitor.visit(integerLiteral: NodeID(rawValue: rawValue))
    case .lambdaExpr:
      return visitor.visit(lambda: NodeID(rawValue: rawValue))
    case .mapLiteralExpr:
      return visitor.visit(mapLiteral: NodeID(rawValue: rawValue))
    case .matchExpr:
      return visitor.visit(match: NodeID(rawValue: rawValue))
    case .nameExpr:
      return visitor.visit(name: NodeID(rawValue: rawValue))
    case .nilExpr:
      return visitor.visit(nil: NodeID(rawValue: rawValue))
    case .sequenceExpr:
      return visitor.visit(sequence: NodeID(rawValue: rawValue))
    case .storedProjectionExpr:
      return visitor.visit(storedProjection: NodeID(rawValue: rawValue))
    case .stringLiteralExpr:
      return visitor.visit(stringLiteral: NodeID(rawValue: rawValue))
    case .subscriptCallExpr:
      return visitor.visit(subscriptCall: NodeID(rawValue: rawValue))
    case .tupleExpr:
      return visitor.visit(tuple: NodeID(rawValue: rawValue))
    case .tupleMemberExpr:
      return visitor.visit(tupleMember: NodeID(rawValue: rawValue))
    case .unicodeScalarLiteralExpr:
      return visitor.visit(unicodeScalarLiteral: NodeID(rawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: ExprID where Subject: Expr {}

/// The type-erased ID of a value expression.
public struct AnyExprID: ExprID {

  /// The underlying type-erased ID.
  var base: AnyNodeID

  /// Creates a type-erased ID from a value expression ID.
  public init<T: ExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
