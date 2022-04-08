import Utils
/// The index of a declaration in an AST.
public struct DeclIndex<T: Decl>: Hashable {

  /// The index of the declaraion.
  let index: Int

  /// Returns a type-erased copy of this index.
  public func erased() -> AnyDeclIndex { AnyDeclIndex(self) }

}

/// The type-erased index of a declaration in an AST.
public struct AnyDeclIndex: Hashable {

  /// The index of the declaraion.
  let index: Int

  /// A unique identifier for the declaration's type.
  let typeID: ObjectIdentifier

  /// Creates a type-erased index from a typed index.
  public init<T>(_ other: DeclIndex<T>) {
    index = other.index
    typeID = ObjectIdentifier(T.self)
  }

  /// Returns a typed copy of this this index.
  public func assumingBound<T: Decl>(to: T.Type) -> DeclIndex<T> {
    precondition(typeID == ObjectIdentifier(T.self))
    return DeclIndex(index: index)
  }

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch typeID {
    case ObjectIdentifier(AssociatedTypeDecl.self):
      return visitor.visit(associatedType: DeclIndex(index: index))
    case ObjectIdentifier(BindingDecl.self):
      return visitor.visit(binding: DeclIndex(index: index))
    case ObjectIdentifier(ExtensionDecl.self):
      return visitor.visit(extension: DeclIndex(index: index))
    case ObjectIdentifier(FunDecl.self):
      return visitor.visit(fun: DeclIndex(index: index))
    case ObjectIdentifier(GenericTypeParamDecl.self):
      return visitor.visit(genericTypeParam: DeclIndex(index: index))
    case ObjectIdentifier(GenericSizeParamDecl.self):
      return visitor.visit(genericSizeParam: DeclIndex(index: index))
    case ObjectIdentifier(MethodImplDecl.self):
      return visitor.visit(methodImpl: DeclIndex(index: index))
    case ObjectIdentifier(ModuleDecl.self):
      return visitor.visit(module: DeclIndex(index: index))
    case ObjectIdentifier(NamespaceDecl.self):
      return visitor.visit(namespace: DeclIndex(index: index))
    case ObjectIdentifier(ParamDecl.self):
      return visitor.visit(param: DeclIndex(index: index))
    case ObjectIdentifier(ProductTypeDecl.self):
      return visitor.visit(productType: DeclIndex(index: index))
    case ObjectIdentifier(SubscriptDecl.self):
      return visitor.visit(subscript: DeclIndex(index: index))
    case ObjectIdentifier(SubscriptImplDecl.self):
      return visitor.visit(subscriptImpl: DeclIndex(index: index))
    case ObjectIdentifier(TraitDecl.self):
      return visitor.visit(trait: DeclIndex(index: index))
    case ObjectIdentifier(TypeAliasDecl.self):
      return visitor.visit(typeAlias: DeclIndex(index: index))
    case ObjectIdentifier(VarDecl.self):
      return visitor.visit(var: DeclIndex(index: index))
    default:
      unreachable()
    }
  }

}
