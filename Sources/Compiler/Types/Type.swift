/// The (static) type of an entity.
public indirect enum Type: TypeProtocol, Hashable {

  case associatedType(AssociatedType)

  case error(ErrorType)

  case existential(ExistentialType)

  case genericSizeParam(GenericSizeParamType)

  case genericTypeParam(GenericTypeParamType)

  case module(ModuleType)

  case trait(TraitType)

  case union(UnionType)

  /// The associated value of this type.
  public var base: TypeProtocol {
    switch self {
    case let .associatedType(t):    return t
    case let .error(t):             return t
    case let .existential(t):       return t
    case let .genericSizeParam(t):  return t
    case let .genericTypeParam(t):  return t
    case let .module(t):            return t
    case let .trait(t):             return t
    case let .union(t):             return t
    }
  }

  public var flags: TypeFlags { base.flags }

  public func canonical() -> Type { base.canonical() }

  /// Returns a textual description of that type.
  public func describe(in ast: AST) -> String { base.describe(in: ast) }

  /// Indicates whether this type is a generic type parameter or associated type.
  public var isTypeParam: Bool {
    switch self {
    case .genericSizeParam, .genericTypeParam:
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

infix operator ~ : ComparisonPrecedence

extension Type {

  /// Returns whether two types are equivalent.
  public static func ~ (l: Self, r: Self) -> Bool {
    l.canonical() == r.canonical()
  }

}
