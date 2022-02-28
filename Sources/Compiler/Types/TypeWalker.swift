/// A `TypeWalker` action to perform after a `willVisit` event.
public enum TypeWalkerAction {

  case stepInto(ValType)

  case stepOver(ValType)

}

/// A base class to implement "event-based" type visitors and transformers.
public protocol TypeWalker: TypeVisitor where Result == ValType {

  /// The parent of the type being visited.
  var parent: ValType? { get set }

  // MARK: Settings

  /// A flag that indicates whether the walker should skip built-in types.
  ///
  /// If this is set, built-in types will be returned "as-is", without triggering `willVisit(_:)`
  /// nor `didVisit(_:)`.
  var shouldSkipBuiltinTypes: Bool { get }

  // MARK: Event handlers

  mutating func willVisit(_ type: ValType) -> TypeWalkerAction

  /// This method is called after the walker visited a type.
  mutating func didVisit(_ type: ValType) -> ValType

}

extension TypeWalker {

  public var shouldSkipBuiltinTypes: Bool { false }

  public mutating func walk(_ type: ValType) -> ValType {
    guard !(type is BuiltinType) || shouldSkipBuiltinTypes else { return type }

    // Fire the `willVisit` event.
    switch willVisit(type) {
    case .stepInto(let substitute):
      // Visit the type's children.
      return didVisit(substitute.accept(&self))

    case .stepOver(let substitute):
      return didVisit(substitute)
    }
  }

  /// This method is called when the walker is about to visit a type.
  public mutating func willVisit(_ type: ValType) -> TypeWalkerAction {
    return .stepInto(type)
  }

  /// This method is called after the walker visited a type.
  public mutating func didVisit(_ type: ValType) -> ValType {
    return type
  }

  // MARK: Traversal

  public mutating func visit(_ type: KindType) -> ValType {
    return walk(type.type).kind
  }

  public mutating func visit(_ type: BuiltinType) -> ValType {
    return type
  }

  public mutating func visit(_ type: BuiltinPointerType) -> ValType {
    return type
  }

  public mutating func visit(_ type: BuiltinIntLiteralType) -> ValType {
    return type
  }

  public mutating func visit(_ type: BuiltinIntType) -> ValType {
    return type
  }

  mutating func visit(_ type: ModuleType) -> ValType {
    return type
  }

  mutating func visit(_ type: NamespaceType) -> ValType {
    return type
  }

  mutating func visit(_ type: ProductType) -> ValType {
    return type
  }

  mutating func visit(_ type: ViewType) -> ValType {
    return type
  }

  mutating func visit(_ type: AliasType) -> ValType {
    return type
  }

  mutating func visit(_ type: ViewCompositionType) -> ValType {
    return type.context.viewCompositionType(type.views.map({ view in
      walk(view) as! ViewType
    }))
  }

  mutating func visit(_ type: UnionType) -> ValType {
    return type.context.unionType(type.elems.map({ elem in
      walk(elem)
    }))
  }

  mutating func visit(_ type: BoundGenericType) -> ValType {
    return type.context.boundGenericType(decl: type.decl, args: type.args.map({ self.walk($0) }))
  }

  mutating func visit(_ type: GenericParamType) -> ValType {
    return type
  }

  mutating func visit(_ type: AssocType) -> ValType {
    return type.context.assocType(interface: type.interface, base: walk(type.base))
  }

  mutating func visit(_ type: SkolemType) -> ValType {
    return type
  }

  mutating func visit(_ type: WitnessType) -> ValType {
    return type
  }

  mutating func visit(_ type: TupleType) -> ValType {
    return type.context.tupleType(type.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: walk(elem.type))
    }))
  }

  mutating func visit(_ type: FunType) -> ValType {
    return type.context.funType(
      params: type.params.map({ $0.map({ self.walk($0) }) }),
      retType: walk(type.retType))
  }

  mutating func visit(_ type: FunParamType) -> ValType {
    return type.context.funParamType(policy: type.policy, rawType: walk(type.rawType))
  }

  mutating func visit(_ type: AsyncType) -> ValType {
    return type.context.asyncType(of: walk(type.base))
  }

  mutating func visit(_ type: UnresolvedType) -> ValType {
    return type
  }

  mutating func visit(_ type: ErrorType) -> ValType {
    return type
  }

  mutating func visit(_ type: TypeVar) -> ValType {
    return type
  }

}
