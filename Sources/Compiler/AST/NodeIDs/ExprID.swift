import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension ExprID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .assignExpr:
      return visitor.visit(assign: NodeID(unsafeRawValue: rawValue))
    case .asyncExpr:
      return visitor.visit(async: NodeID(unsafeRawValue: rawValue))
    case .awaitExpr:
      return visitor.visit(await: NodeID(unsafeRawValue: rawValue))
    case .boolLiteralExpr:
      return visitor.visit(boolLiteral: NodeID(unsafeRawValue: rawValue))
    case .charLiteralExpr:
      return visitor.visit(charLiteral: NodeID(unsafeRawValue: rawValue))
    case .castExpr:
      return visitor.visit(cast: NodeID(unsafeRawValue: rawValue))
    case .condExpr:
      return visitor.visit(cond: NodeID(unsafeRawValue: rawValue))
    case .floatLiteralExpr:
      return visitor.visit(floatLiteral: NodeID(unsafeRawValue: rawValue))
    case .funCallExpr:
      return visitor.visit(funCall: NodeID(unsafeRawValue: rawValue))
    case .integerLiteralExpr:
      return visitor.visit(integerLiteral: NodeID(unsafeRawValue: rawValue))
    case .lambdaExpr:
      return visitor.visit(lambda: NodeID(unsafeRawValue: rawValue))
    case .mapLiteralExpr:
      return visitor.visit(mapLiteral: NodeID(unsafeRawValue: rawValue))
    case .matchExpr:
      return visitor.visit(match: NodeID(unsafeRawValue: rawValue))
    case .matchCaseExpr:
      return visitor.visit(match: NodeID(unsafeRawValue: rawValue))
    case .nameExpr:
      return visitor.visit(name: NodeID(unsafeRawValue: rawValue))
    case .nilExpr:
      return visitor.visit(nil: NodeID(unsafeRawValue: rawValue))
    case .sequenceExpr:
      return visitor.visit(sequence: NodeID(unsafeRawValue: rawValue))
    case .storedProjectionExpr:
      return visitor.visit(storedProjection: NodeID(unsafeRawValue: rawValue))
    case .stringLiteralExpr:
      return visitor.visit(stringLiteral: NodeID(unsafeRawValue: rawValue))
    case .subscriptCallExpr:
      return visitor.visit(subscriptCall: NodeID(unsafeRawValue: rawValue))
    case .tupleExpr:
      return visitor.visit(tuple: NodeID(unsafeRawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: ExprID where T: Expr {}

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
