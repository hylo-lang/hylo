/// A type that implements a visitation method for each kind of declaration node.
public protocol DeclVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(associatedSize: NodeIndex<AssociatedSizeDecl>) -> Result

  mutating func visit(associatedType: NodeIndex<AssociatedTypeDecl>) -> Result

  mutating func visit(binding: NodeIndex<BindingDecl>) -> Result

  mutating func visit(conformance: NodeIndex<ConformanceDecl>) -> Result

  mutating func visit(extension: NodeIndex<ExtensionDecl>) -> Result

  mutating func visit(fun: NodeIndex<FunDecl>) -> Result

  mutating func visit(genericSizeParam: NodeIndex<GenericSizeParamDecl>) -> Result

  mutating func visit(genericTypeParam: NodeIndex<GenericTypeParamDecl>) -> Result

  mutating func visit(methodImpl: NodeIndex<MethodImplDecl>) -> Result

  mutating func visit(module: NodeIndex<ModuleDecl>) -> Result

  mutating func visit(namespace: NodeIndex<NamespaceDecl>) -> Result

  mutating func visit(param: NodeIndex<ParamDecl>) -> Result

  mutating func visit(productType: NodeIndex<ProductTypeDecl>) -> Result

  mutating func visit(subscript: NodeIndex<SubscriptDecl>) -> Result

  mutating func visit(subscriptImpl: NodeIndex<SubscriptImplDecl>) -> Result

  mutating func visit(trait: NodeIndex<TraitDecl>) -> Result

  mutating func visit(typeAlias: NodeIndex<TypeAliasDecl>) -> Result

  mutating func visit(`var`: NodeIndex<VarDecl>) -> Result

}
