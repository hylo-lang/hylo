/// Base protocol for types implementing exhaustive type visitation.
public protocol TypeVisitor {

  associatedtype Result

  mutating func visit(_ type: KindType) -> Result

  mutating func visit(_ type: BuiltinType) -> Result

  mutating func visit(_ type: BuiltinPointerType) -> Result

  mutating func visit(_ type: BuiltinIntLiteralType) -> Result

  mutating func visit(_ type: BuiltinIntType) -> Result

  mutating func visit(_ type: ModuleType) -> Result

  mutating func visit(_ type: NamespaceType) -> Result

  mutating func visit(_ type: ProductType) -> Result

  mutating func visit(_ type: ViewType) -> Result

  mutating func visit(_ type: AliasType) -> Result

  mutating func visit(_ type: ViewCompositionType) -> Result

  mutating func visit(_ type: UnionType) -> Result

  mutating func visit(_ type: BoundGenericType) -> Result

  mutating func visit(_ type: GenericParamType) -> Result

  mutating func visit(_ type: AssocType) -> Result

  mutating func visit(_ type: SkolemType) -> Result

  mutating func visit(_ type: WitnessType) -> Result

  mutating func visit(_ type: TupleType) -> Result

  mutating func visit(_ type: FunType) -> Result

  mutating func visit(_ type: FunParamType) -> Result

  mutating func visit(_ type: AsyncType) -> Result

  mutating func visit(_ type: UnresolvedType) -> Result

  mutating func visit(_ type: ErrorType) -> Result

  mutating func visit(_ type: TypeVar) -> Result

}
