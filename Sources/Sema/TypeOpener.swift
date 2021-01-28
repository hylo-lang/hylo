import AST

/// A visitor that creates the opened form of a type, where fresh variables have been susbtituted
/// for all generic type parameters.
final class TypeOpener: TypeVisitor {

  typealias Result = ValType

  private var substitutions: [ObjectIdentifier: TypeVar] = [:]

  func visit(_ type: KindType) -> ValType {
    return type.props.contains(.hasTypeParams)
      ? type
      : type.type.accept(self).kind
  }

  func visit(_ type: BuiltinType) -> ValType {
    return type
  }

  func visit(_ type: BuiltinIntLiteralType) -> ValType {
    return type
  }

  func visit(_ type: BuiltinIntType) -> ValType {
    return type
  }

  func visit(_ type: ModuleType) -> ValType {
    return type
  }

  func visit(_ type: ProductType) -> ValType {
    return type
  }

  func visit(_ type: ViewType) -> ValType {
    return type
  }

  func visit(_ type: GenericParamType) -> ValType {
    if let subst = substitutions[ObjectIdentifier(type)] {
      return subst
    }

    let subst = TypeVar(context: type.context, node: type.decl)
    substitutions[ObjectIdentifier(type)] = subst
    return subst
  }

  func visit(_ type: ExistentialType) -> ValType {
    return type
  }

  func visit(_ type: TupleType) -> ValType {
    guard type.props.contains(.hasTypeParams) else { return type }

    return type.context.tupleType(type.elems.map(({ elem in
      TupleType.Elem(label: elem.label, type: elem.type.accept(self))
    })))
  }

  func visit(_ type: FunType) -> ValType {
    guard type.props.contains(.hasTypeParams) else { return type }

    return type.context.funType(
      paramType: type.paramType.accept(self), retType: type.paramType.accept(self))
  }

  func visit(_ type: InoutType) -> ValType {
    guard type.props.contains(.hasTypeParams) else { return type }

    return type.context.inoutType(of: type.base.accept(self))
  }

  func visit(_ type: UnresolvedType) -> ValType {
    return type
  }

  func visit(_ type: ErrorType) -> ValType {
    return type
  }

  func visit(_ type: TypeVar) -> ValType {
    return type
  }

}

extension ValType {

  /// The opened form of this type.
  var opened: ValType {
    let opener = TypeOpener()
    return accept(opener)
  }

}
