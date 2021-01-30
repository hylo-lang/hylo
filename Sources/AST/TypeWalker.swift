/// A base class to implement "event-based" type visitors and transformers.
open class TypeWalker: TypeVisitor {

  public typealias Result = ValType

  public enum Action {

    case stepInto(ValType)

    case stepOver(ValType)

  }

  /// The parent of the type being visited.
  public final private(set) var parent: ValType?

  // MARK: Settings

  /// A flag that indicates whether the walker should skip built-in types.
  ///
  /// If this is set, built-in types will be returned "as-is", without triggering `willVisit(_:)`
  /// nor `didVisit(_:)`.
  open var shouldSkipBuiltinTypes: Bool { true }

  // MARK: Event handlers

  /// This method is called when the walker is about to visit a type.
  open func willVisit(_ type: ValType) -> Action {
    return .stepInto(type)
  }

  /// This method is called after the walker visited a type.
  open func didVisit(_ type: ValType) -> ValType {
    return type
  }

  // MARK: Traversal

  open func visit(_ type: KindType) -> ValType {
    return walk(type.type).kind
  }

  open func visit(_ type: BuiltinType) -> ValType {
    return type
  }

  open func visit(_ type: BuiltinIntLiteralType) -> ValType {
    return type
  }

  open func visit(_ type: BuiltinIntType) -> ValType {
    return type
  }

  open func visit(_ type: ModuleType) -> ValType {
    return type
  }

  open func visit(_ type: ProductType) -> ValType {
    return type
  }

  open func visit(_ type: ViewType) -> ValType {
    return type
  }

  open func visit(_ type: GenericParamType) -> ValType {
    return type
  }

  open func visit(_ type: ExistentialType) -> ValType {
    return type
  }

  open func visit(_ type: TupleType) -> ValType {
    return type.context.tupleType(type.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: walk(elem.type))
    }))
  }

  open func visit(_ type: FunType) -> ValType {
    return type.context.funType(
      paramType: walk(type.paramType), retType: walk(type.retType))
  }

  open func visit(_ type: InoutType) -> ValType {
    return type.context.inoutType(of: walk(type.base))
  }

  open func visit(_ type: UnresolvedType) -> ValType {
    return type
  }

  open func visit(_ type: ErrorType) -> ValType {
    return type
  }

  open func visit(_ type: TypeVar) -> ValType {
    return type
  }

  public final func walk(_ type: ValType) -> ValType {
    // Fire the `willVisit` event.
    switch willVisit(type) {
    case .stepInto(let substitute):
      // Visit the type's children.
      return didVisit(substitute.accept(self))

    case .stepOver(let substitute):
      return didVisit(substitute)
    }
  }

}
