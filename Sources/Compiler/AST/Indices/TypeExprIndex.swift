import Utils

/// The index of a type expression.
public protocol TypeExprIndex: NodeIndexProtocol {}

extension NodeIndex: TypeExprIndex where T: TypeExpr {}

/// The type-erased index of a type expression.
public struct AnyTypeExprIndex: TypeExprIndex {

  /// The underlying type-erased index.
  public private(set) var base: AnyNodeIndex

  /// Creates a type-erased index from a type expression index.
  public init<T: TypeExprIndex>(_ other: T) {
    base = AnyNodeIndex(other)
  }

  public var rawValue: Int { base.rawValue }

  public var typeID: ObjectIdentifier { base.typeID }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: TypeExpr>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    switch base.typeID {
    case AsyncTypeExpr.typeID:
      return visitor.visit(async: NodeIndex(rawValue: base.rawValue))
    case ConformanceLensTypeExpr.typeID:
      return visitor.visit(conformanceLens: NodeIndex(rawValue: base.rawValue))
    case ExistentialTypeExpr.typeID:
      return visitor.visit(existential: NodeIndex(rawValue: base.rawValue))
    case IndirectTypeExpr.typeID:
      return visitor.visit(indirect: NodeIndex(rawValue: base.rawValue))
    case LambdaTypeExpr.typeID:
      return visitor.visit(lambda: NodeIndex(rawValue: base.rawValue))
    case NameTypeExpr.typeID:
      return visitor.visit(name: NodeIndex(rawValue: base.rawValue))
    case ParamTypeExpr.typeID:
      return visitor.visit(param: NodeIndex(rawValue: base.rawValue))
    case StoredProjectionTypeExpr.typeID:
      return visitor.visit(storedProjection: NodeIndex(rawValue: base.rawValue))
    case TupleTypeExpr.typeID:
      return visitor.visit(tuple: NodeIndex(rawValue: base.rawValue))
    case UnionTypeExpr.typeID:
      return visitor.visit(union: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
