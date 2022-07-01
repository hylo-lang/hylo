/// A type that implements a visitation method for each kind of declaration node.
public protocol DeclVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(associatedSize: NodeID<AssociatedSizeDecl>) -> Result

  mutating func visit(associatedType: NodeID<AssociatedTypeDecl>) -> Result

  mutating func visit(binding: NodeID<BindingDecl>) -> Result

  mutating func visit(conformance: NodeID<ConformanceDecl>) -> Result

  mutating func visit(extension: NodeID<ExtensionDecl>) -> Result

  mutating func visit(fun: NodeID<FunDecl>) -> Result

  mutating func visit(genericSizeParam: NodeID<GenericSizeParamDecl>) -> Result

  mutating func visit(genericTypeParam: NodeID<GenericTypeParamDecl>) -> Result

  mutating func visit(methodImpl: NodeID<MethodImplDecl>) -> Result

  mutating func visit(module: NodeID<ModuleDecl>) -> Result

  mutating func visit(namespace: NodeID<NamespaceDecl>) -> Result

  mutating func visit(operator: NodeID<OperatorDecl>) -> Result

  mutating func visit(param: NodeID<ParameterDecl>) -> Result

  mutating func visit(productType: NodeID<ProductTypeDecl>) -> Result

  mutating func visit(subscript: NodeID<SubscriptDecl>) -> Result

  mutating func visit(subscriptImpl: NodeID<SubscriptImplDecl>) -> Result

  mutating func visit(trait: NodeID<TraitDecl>) -> Result

  mutating func visit(typeAlias: NodeID<TypeAliasDecl>) -> Result

  mutating func visit(`var`: NodeID<VarDecl>) -> Result

}
