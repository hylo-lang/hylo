/// A type that implements a visitation method for each kind of declaration node.
public protocol DeclVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(associatedType: AssociatedTypeDecl) -> Result

  mutating func visit(binding: BindingDecl) -> Result

  mutating func visit(conformance: ConformanceDecl) -> Result

  mutating func visit(extension: ExtensionDecl) -> Result

  mutating func visit(fun: FunDecl) -> Result

  mutating func visit(genericSizeParam: GenericSizeParamDecl) -> Result

  mutating func visit(genericTypeParam: GenericTypeParamDecl) -> Result

  mutating func visit(methodImpl: MethodImplDecl) -> Result

  mutating func visit(module: ModuleDecl) -> Result

  mutating func visit(namespace: NamespaceDecl) -> Result

  mutating func visit(param: ParamDecl) -> Result

  mutating func visit(productType: ProductTypeDecl) -> Result

  mutating func visit(subscript: SubscriptDecl) -> Result

  mutating func visit(subscriptImpl: SubscriptImplDecl) -> Result

  mutating func visit(trait: TraitDecl) -> Result

  mutating func visit(typeAlias: TypeAliasDecl) -> Result

  mutating func visit(`var`: VarDecl) -> Result

}
