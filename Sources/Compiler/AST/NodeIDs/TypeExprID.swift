import Utils

/// The ID of a type expression.
public protocol TypeExprID: NodeIDProtocol {}

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

  /// Returns a typed copy of this ID, or `nil` if the type conversion failed.
  public func convert<T: TypeExpr>(to: T.Type) -> NodeID<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case AsyncTypeExpr.kind:
      return visitor.visit(async: NodeID(rawValue: base.rawValue))
    case ConformanceLensTypeExpr.kind:
      return visitor.visit(conformanceLens: NodeID(rawValue: base.rawValue))
    case ExistentialTypeExpr.kind:
      return visitor.visit(existential: NodeID(rawValue: base.rawValue))
    case IndirectTypeExpr.kind:
      return visitor.visit(indirect: NodeID(rawValue: base.rawValue))
    case LambdaTypeExpr.kind:
      return visitor.visit(lambda: NodeID(rawValue: base.rawValue))
    case NameTypeExpr.kind:
      return visitor.visit(name: NodeID(rawValue: base.rawValue))
    case ParamTypeExpr.kind:
      return visitor.visit(param: NodeID(rawValue: base.rawValue))
    case StoredProjectionTypeExpr.kind:
      return visitor.visit(storedProjection: NodeID(rawValue: base.rawValue))
    case TupleTypeExpr.kind:
      return visitor.visit(tuple: NodeID(rawValue: base.rawValue))
    case UnionTypeExpr.kind:
      return visitor.visit(union: NodeID(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
