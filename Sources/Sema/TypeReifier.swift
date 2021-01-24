import AST

final class TypeReifier: TypeVisitor {

  typealias Result = ValType

  init(substitutions: [TypeVar: ValType]) {
    self.substitutions = substitutions
  }

  let substitutions: [TypeVar: ValType]

  func visit(_ type: KindType) -> ValType {
    return type.type.accept(self).kind
  }

  func visit(_ type: BuiltinType) -> ValType {
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

  func visit(_ type: TupleType) -> ValType {
    return type.context
      .tupleType(type.elems.map({ elem in
        TupleType.Elem(label: elem.label, type: elem.type.accept(self))
      }))
      .canonical
  }

  func visit(_ type: FunType) -> ValType {
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

  func visit(_ type: TypeVar) -> ValType {
    return substitutions[type]?.accept(self) ?? type
  }

}
