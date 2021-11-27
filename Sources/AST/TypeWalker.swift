/// A base class to implement "event-based" type visitors and transformers.
open class TypeWalker: TypeVisitor {

  public init() {}

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
  open var shouldSkipBuiltinTypes: Bool { false }

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

  open func visit(_ type: AliasType) -> ValType {
    return type
  }

  open func visit(_ type: ViewCompositionType) -> ValType {
    return type.context.viewCompositionType(type.views.map({ view in
      walk(view) as! ViewType
    }))
  }

  open func visit(_ type: UnionType) -> ValType {
    return type.context.unionType(type.elems.map({ elem in
      walk(elem)
    }))
  }

  open func visit(_ type: BoundGenericType) -> ValType {
    return type.context.boundGenericType(decl: type.decl, args: type.args.map(walk(_:)))
  }

  open func visit(_ type: GenericParamType) -> ValType {
    return type
  }

  open func visit(_ type: AssocType) -> ValType {
    return type.context.assocType(interface: type.interface, base: walk(type.base))
  }

  open func visit(_ type: SkolemType) -> ValType {
    return type
  }

  open func visit(_ type: TupleType) -> ValType {
    return type.context.tupleType(type.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: walk(elem.type))
    }))
  }

  open func visit(_ type: FunType) -> ValType {
    return type.context.funType(paramType: walk(type.paramType), retType: walk(type.retType))
  }

  open func visit(_ type: AsyncType) -> ValType {
    return type.context.asyncType(of: walk(type.base))
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
    guard !(type is BuiltinType) || shouldSkipBuiltinTypes else { return type }

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

/// A type walker whose event handlers are built from closures.
public final class FunctionalTypeWalker: TypeWalker {

  public init(
    willVisit willVisitHandler: @escaping (ValType) -> TypeWalker.Action = { .stepInto($0) },
    didVisit didVisitHandler: @escaping (ValType) -> ValType = { $0 }
  ) {
    self.willVisitHandler = willVisitHandler
    self.didVisitHandler = didVisitHandler
  }

  private let willVisitHandler: (ValType) -> TypeWalker.Action

  private let didVisitHandler: (ValType) -> ValType

  public override func willVisit(_ type: ValType) -> TypeWalker.Action {
    return willVisitHandler(type)
  }

  public override func didVisit(_ type: ValType) -> ValType {
    return didVisitHandler(type)
  }

}
