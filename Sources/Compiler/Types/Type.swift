import Utils

/// The (static) type of an entity.
public indirect enum Type: TypeProtocol, Hashable {

  case associatedType(AssociatedType)

  case associatedValue(AssociatedValueType)

  case boundGeneric(BoundGenericType)

  case builtin(BuiltinType)

  case conformanceLens(ConformanceLensType)

  case error(ErrorType)

  case existential(ExistentialType)

  case genericTypeParam(GenericTypeParamType)

  case genericValueParam(GenericValueParamType)

  case lambda(LambdaType)

  case metaType(MetaTypeType)

  case method(MethodType)

  case module(ModuleType)

  case parameter(ParameterType)

  case product(ProductType)

  case remote(RemoteType)

  case skolem(SkolemType)

  case `subscript`(SubscriptType)

  case trait(TraitType)

  case tuple(TupleType)

  case typeAlias(TypeAliasType)

  case union(UnionType)

  case variable(TypeVariable)

  /// The associated value of this type.
  public var base: TypeProtocol {
    switch self {
    case let .associatedType(t):    return t
    case let .associatedValue(t):   return t
    case let .boundGeneric(t):      return t
    case let .builtin(t):           return t
    case let .conformanceLens(t):   return t
    case let .error(t):             return t
    case let .existential(t):       return t
    case let .genericTypeParam(t):  return t
    case let .genericValueParam(t): return t
    case let .lambda(t):            return t
    case let .metaType(t):          return t
    case let .method(t):            return t
    case let .module(t):            return t
    case let .parameter(t):         return t
    case let .product(t):           return t
    case let .remote(t):            return t
    case let .skolem(t):            return t
    case let .subscript(t):         return t
    case let .trait(t):             return t
    case let .tuple(t):             return t
    case let .typeAlias(t):         return t
    case let .union(t):             return t
    case let .variable(t):          return t
    }
  }

  public var flags: TypeFlags { base.flags }

  /// Returns `self` if it is not the error type. Otherwise, returns `nil`.
  public var proper: Type? {
    if case .error = self { return nil } else { return self }
  }

  /// Indicates whether the type is the error type.
  public var isError: Bool {
    proper == nil
  }

  /// Indicates whether the type is a variable.
  public var isVariable: Bool {
    if case .variable = self { return true } else { return false }
  }

  /// Indicates whether the type is a leaf.
  ///
  /// A leaf type is a type whose only subtypes are itself and `Never`.
  public var isLeaf: Bool {
    switch self {
    case .existential, .lambda, .variable:
      return false
    case .union(let t):
      return t.elements.isEmpty
    default:
      return true
    }
  }

  /// Indicates whether the type is a generic type parameter or associated type.
  public var isTypeParam: Bool {
    switch self {
    case .associatedType, .genericTypeParam:
      return true
    default:
      return false
    }
  }

  /// Indicates whether `type` has a record layout.
  public var hasRecordLayout: Bool {
    switch self {
    case .product, .tuple:
      return true
    case .boundGeneric(let type):
      return type.base.hasRecordLayout
    default:
      return false
    }
  }

  /// The `Any` type.
  public static let any: Type = .existential(ExistentialType(traits: [], constraints: []))

  /// The `Never` type.
  public static let never: Type = .union(UnionType([]))

  /// The void type (i.e., `{}`).
  public static let void: Type = .tuple(TupleType([]))

}

extension Type {

  /// The result of a call to the transformer of `transform(_:)`.
  public enum TransformAction {

    case stepInto(Type)

    case stepOver(Type)

  }

  /// The skolemized form of this type.
  ///
  /// A type is skolemized when all its generic parameters have been substituted for skolems.
  public var skolemized: Type {
    transform({ type in
      switch type {
      case .associatedType,
           .genericTypeParam:
        return .stepOver(.skolem(SkolemType(base: type)))

      case .associatedValue,
           .genericValueParam:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParam] || type[.hasGenericValueParam] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    })
  }

  /// Returns this type transformed with `transformer`.
  ///
  /// This method visits the structure of the type and calls `transformer` on each type composing
  /// that structure. The result of the call substitutes the visited type. If `transformer` returns
  /// `stepInto(t)`, `t` is visited after the substitution. Otherwise, the method directly moves to
  /// the next type in the structure.
  public func transform(_ transformer: (Type) -> TransformAction) -> Type {
    switch transformer(self) {
    case .stepInto(let type):
      switch type {
      case .associatedType,
           .associatedValue,
           .builtin,
           .error,
           .existential,
           .genericTypeParam,
           .genericValueParam,
           .module,
           .product,
           .skolem,
           .trait,
           .typeAlias,
           .variable:
        return type

      case .boundGeneric(let type):
        return .boundGeneric(BoundGenericType(
          type.base.transform(transformer),
          arguments: type.arguments.map({ a in
            switch a {
            case .type(let type):
              return .type(type.transform(transformer))
            case .value:
              fatalError("not implemented")
            }
          })))

      case .conformanceLens(let type):
        return .conformanceLens(ConformanceLensType(
          wrapped: type.wrapped.transform(transformer),
          focus: type.focus))

      case .lambda(let type):
        return .lambda(LambdaType(
          receiverEffect: type.receiverEffect,
          environment: type.environment.transform(transformer),
          inputs: type.inputs.map({ p in
            CallableTypeParameter(
              label: p.label,
              type: p.type.transform(transformer))
          }),
          output: type.output.transform(transformer)))

      case .metaType(let type):
        return .metaType(MetaTypeType(type.instance.transform(transformer)))

      case .method(let type):
        return .method(MethodType(
          capabilities: type.capabilities,
          receiver: type.receiver.transform(transformer),
          inputs: type.inputs.map({ p in
            CallableTypeParameter(
              label: p.label,
              type: p.type.transform(transformer))
          }),
          output: type.output.transform(transformer)))

      case .parameter(let type):
        return .parameter(ParameterType(
          convention: type.convention,
          bareType: type.bareType.transform(transformer)))

      case .remote(let type):
        return .remote(RemoteType(
          type.capability,
          type.base.transform(transformer)))

      case .subscript(let type):
        return .subscript(SubscriptType(
          isProperty: type.isProperty,
          capabilities: type.capabilities,
          inputs: type.inputs.map({ p in
            CallableTypeParameter(
              label: p.label,
              type: p.type.transform(transformer))
          }),
          output: type.output.transform(transformer)))

      case .tuple(let type):
        return .tuple(TupleType(
          type.elements.map({ e in
            TupleType.Element(
              label: e.label,
              type: e.type.transform(transformer))
          })))

      case .union(let type):
        return .union(UnionType(type.elements.map({ e in e.transform(transformer) })))
      }

    case .stepOver(let type):
      return type
    }
  }

}

extension Type: CustomStringConvertible {

  public var description: String { "\(base)" }

}
