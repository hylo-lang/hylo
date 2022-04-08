/// A type that implements a visitation method for each kind of declaration node.
public protocol DeclVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(associatedType: DeclIndex<AssociatedTypeDecl>) -> Result

  mutating func visit(binding: DeclIndex<BindingDecl>) -> Result

  mutating func visit(conformance: DeclIndex<ConformanceDecl>) -> Result

  mutating func visit(extension: DeclIndex<ExtensionDecl>) -> Result

  mutating func visit(fun: DeclIndex<FunDecl>) -> Result

  mutating func visit(genericSizeParam: DeclIndex<GenericSizeParamDecl>) -> Result

  mutating func visit(genericTypeParam: DeclIndex<GenericTypeParamDecl>) -> Result

  mutating func visit(methodImpl: DeclIndex<MethodImplDecl>) -> Result

  mutating func visit(module: DeclIndex<ModuleDecl>) -> Result

  mutating func visit(namespace: DeclIndex<NamespaceDecl>) -> Result

  mutating func visit(param: DeclIndex<ParamDecl>) -> Result

  mutating func visit(productType: DeclIndex<ProductTypeDecl>) -> Result

  mutating func visit(subscript: DeclIndex<SubscriptDecl>) -> Result

  mutating func visit(subscriptImpl: DeclIndex<SubscriptImplDecl>) -> Result

  mutating func visit(trait: DeclIndex<TraitDecl>) -> Result

  mutating func visit(typeAlias: DeclIndex<TypeAliasDecl>) -> Result

  mutating func visit(`var`: DeclIndex<VarDecl>) -> Result

}
