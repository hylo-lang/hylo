import Utils

/// The ID of a type expression.
public protocol TypeExprID: NodeIDProtocol {}

extension TypeExprID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .asyncTypeExpr:
      return visitor.visit(async: NodeID(unsafeRawValue: rawValue))
    case .conformanceLensTypeExpr:
      return visitor.visit(conformanceLens: NodeID(unsafeRawValue: rawValue))
    case .existentialTypeExpr:
      return visitor.visit(existential: NodeID(unsafeRawValue: rawValue))
    case .indirectTypeExpr:
      return visitor.visit(indirect: NodeID(unsafeRawValue: rawValue))
    case .lambdaTypeExpr:
      return visitor.visit(lambda: NodeID(unsafeRawValue: rawValue))
    case .nameTypeExpr:
      return visitor.visit(name: NodeID(unsafeRawValue: rawValue))
    case .paramTypeExpr:
      return visitor.visit(param: NodeID(unsafeRawValue: rawValue))
    case .storedProjectionTypeExpr:
      return visitor.visit(storedProjection: NodeID(unsafeRawValue: rawValue))
    case .tupleTypeExpr:
      return visitor.visit(tuple: NodeID(unsafeRawValue: rawValue))
    case .unionTypeExpr:
      return visitor.visit(union: NodeID(unsafeRawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: TypeExprID where T: TypeExpr {}

/// The type-erased ID of a type expression.
public struct AnyTypeExprID: TypeExprID {

  /// The underlying type-erased ID.
  public private(set) var base: AnyNodeID

  /// Creates a type-erased ID from a type expression ID.
  public init<T: TypeExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
