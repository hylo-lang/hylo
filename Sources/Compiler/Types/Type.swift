/// The (static) type of an entity.
public indirect enum Type: TypeProtocol, Hashable {

  case associated(AssociatedType)

  case boundGeneric(BoundGenericType)

  case builtin(BuiltinType)

  case conformanceLens(ConformanceLensType)

  case error(ErrorType)

  case existential(ExistentialType)

  case genericSizeParam(GenericSizeParamType)

  case genericTypeParam(GenericTypeParamType)

  case module(ModuleType)

  case product(ProductType)

  case `subscript`(SubscriptType)

  case trait(TraitType)

  case tuple(TupleType)

  case typeAlias(TypeAliasType)

  case union(UnionType)

  case variable(TypeVariable)

  /// The associated value of this type.
  public var base: TypeProtocol {
    switch self {
    case let .associated(t):        return t
    case let .boundGeneric(t):      return t
    case let .builtin(t):           return t
    case let .conformanceLens(t):   return t
    case let .error(t):             return t
    case let .existential(t):       return t
    case let .genericSizeParam(t):  return t
    case let .genericTypeParam(t):  return t
    case let .module(t):            return t
    case let .product(t):           return t
    case let .subscript(t):         return t
    case let .trait(t):             return t
    case let .tuple(t):             return t
    case let .typeAlias(t):         return t
    case let .union(t):             return t
    case let .variable(t):          return t
    }
  }

  public var flags: TypeFlags { base.flags }

  /// Indicates whether this type is a generic type parameter or associated type.
  public var isTypeParam: Bool {
    switch self {
    case .associated, .genericTypeParam:
      return true
    default:
      return false
    }
  }

  /// The `Any` type.
  public static var any: Type = .existential(ExistentialType(traits: [], constraints: []))

  /// The `Never` type.
  public static var never: Type = .union(UnionType(elements: []))

  /// Returns `Val.Int`, declared in `ast.stdlib`.
  public static func int(in ast: AST) -> Type? {
    ProductType(named: "Int", ast: ast).map({ .product($0) })
  }

  /// Returns `Val.Double`, declared in `ast.stdlib`.
  public static func double(in ast: AST) -> Type? {
    ProductType(named: "Double", ast: ast).map({ .product($0) })
  }

}

extension Type: CustomStringConvertible {

  public var description: String { "\(base)" }

}
