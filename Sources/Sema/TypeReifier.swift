import AST

final class TypeReifier: TypeVisitor {

  typealias Result = ValType

  init(substitutions: [TypeVar: ValType]) {
    self.substitutions = substitutions
  }

  let substitutions: [TypeVar: ValType]

  func visit(_ type: KindType) -> ValType {
    guard type.props.contains(.hasVariables) else { return type }
    return type.type.accept(self).kind
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
    return type
  }

  func visit(_ type: ExistentialType) -> ValType {
    return type
  }

  func visit(_ type: TupleType) -> ValType {
    guard type.props.contains(.hasVariables) else { return type }
    return type.context
      .tupleType(type.elems.map({ elem in
        TupleType.Elem(label: elem.label, type: elem.type.accept(self))
      }))
      .canonical
  }

  func visit(_ type: FunType) -> ValType {
    guard type.props.contains(.hasVariables) else { return type }
    return type.context
      .funType(
        paramType: type.paramType.accept(self),
        retType: type.retType.accept(self))
      .canonical
  }

  func visit(_ type: InoutType) -> ValType {
    return type
  }

  func visit(_ type: UnresolvedType) -> ValType {
    return type
  }

  func visit(_ type: ErrorType) -> ValType {
    return type
  }

  func visit(_ type: TypeVar) -> ValType {
    return substitutions[type]?.accept(self) ?? type
  }

}
