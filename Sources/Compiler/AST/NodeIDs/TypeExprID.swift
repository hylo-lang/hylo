import Utils

/// The ID of a type expression.
public protocol TypeExprID: NodeIDProtocol {}

extension TypeExprID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .asyncTypeExpr:
      return visitor.visit(async: NodeID(rawValue: rawValue))
    case .conformanceLensTypeExpr:
      return visitor.visit(conformanceLens: NodeID(rawValue: rawValue))
    case .existentialTypeExpr:
      return visitor.visit(existential: NodeID(rawValue: rawValue))
    case .indirectTypeExpr:
      return visitor.visit(indirect: NodeID(rawValue: rawValue))
    case .lambdaTypeExpr:
      return visitor.visit(lambda: NodeID(rawValue: rawValue))
    case .nameTypeExpr:
      return visitor.visit(name: NodeID(rawValue: rawValue))
    case .parameterTypeExpr:
      return visitor.visit(param: NodeID(rawValue: rawValue))
    case .storedProjectionTypeExpr:
      return visitor.visit(storedProjection: NodeID(rawValue: rawValue))
    case .tupleTypeExpr:
      return visitor.visit(tuple: NodeID(rawValue: rawValue))
    case .unionTypeExpr:
      return visitor.visit(union: NodeID(rawValue: rawValue))
    case .wildcardTypeExpr:
      return visitor.visit(wildcard: NodeID(rawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: TypeExprID where Subject: TypeExpr {}

/// The type-erased ID of a type expression.
public struct AnyTypeExprID: TypeExprID {

  /// The underlying type-erased ID.
  public let base: AnyNodeID

  /// Creates a type-erased ID from a type expression ID.
  public init<T: TypeExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
