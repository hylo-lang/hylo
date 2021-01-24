/// Base protocol for types implementing exhaustive type visitation.
public protocol TypeVisitor {

  associatedtype Result

  func visit(_ type: KindType) -> Result

  func visit(_ type: BuiltinType) -> Result

  func visit(_ type: BuiltinIntType) -> Result

  func visit(_ type: ModuleType) -> Result

  func visit(_ type: ProductType) -> Result

  func visit(_ type: ViewType) -> Result

  func visit(_ type: TupleType) -> Result

  func visit(_ type: FunType) -> Result

  func visit(_ type: InoutType) -> Result

  func visit(_ type: UnresolvedType) -> Result

  func visit(_ type: TypeVar) -> Result

}
