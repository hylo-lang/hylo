/// The (static) type of an entity.
public indirect enum Type: TypeProtocol, Hashable {

  case associated(AssociatedType)

  case boundGeneric(BoundGenericType)

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

  /// The associated value of this type.
  public var base: TypeProtocol {
    switch self {
    case let .associated(t):        return t
    case let .boundGeneric(t):      return t
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
    }
  }

  public var flags: TypeFlags { base.flags }

  /// Indicates whether this type is a generic type parameter or associated type.
  public var isTypeParam: Bool {
    switch self {
    case .associated, .genericSizeParam, .genericTypeParam:
      return true
    default:
      return false
    }
  }

  /// The `Any` type.
  public static var any: Type = .existential(ExistentialType(traits: [], constraints: []))

  /// The `Never` type.
  public static var never: Type = .union(UnionType(elements: []))

}

extension Type: CustomStringConvertible {

  public var description: String { "\(base)" }

}
