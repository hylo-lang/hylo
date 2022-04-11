import Utils

/// The index of a declaration.
public protocol DeclIndex: NodeIndexProtocol {}

extension NodeIndex: DeclIndex where T: Decl {}

/// The type-erased index of a declaration.
public struct AnyDeclIndex: DeclIndex {

  /// The underlying type-erased index.
  var base: AnyNodeIndex

  /// Creates a type-erased index from a declaration index.
  public init<T: DeclIndex>(_ other: T) {
    base = AnyNodeIndex(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Decl>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case AssociatedSizeDecl.kind:
      return visitor.visit(associatedSize: NodeIndex(rawValue: base.rawValue))
    case AssociatedTypeDecl.kind:
      return visitor.visit(associatedType: NodeIndex(rawValue: base.rawValue))
    case BindingDecl.kind:
      return visitor.visit(binding: NodeIndex(rawValue: base.rawValue))
    case ExtensionDecl.kind:
      return visitor.visit(extension: NodeIndex(rawValue: base.rawValue))
    case FunDecl.kind:
      return visitor.visit(fun: NodeIndex(rawValue: base.rawValue))
    case GenericTypeParamDecl.kind:
      return visitor.visit(genericTypeParam: NodeIndex(rawValue: base.rawValue))
    case GenericSizeParamDecl.kind:
      return visitor.visit(genericSizeParam: NodeIndex(rawValue: base.rawValue))
    case MethodImplDecl.kind:
      return visitor.visit(methodImpl: NodeIndex(rawValue: base.rawValue))
    case ModuleDecl.kind:
      return visitor.visit(module: NodeIndex(rawValue: base.rawValue))
    case NamespaceDecl.kind:
      return visitor.visit(namespace: NodeIndex(rawValue: base.rawValue))
    case ParamDecl.kind:
      return visitor.visit(param: NodeIndex(rawValue: base.rawValue))
    case ProductTypeDecl.kind:
      return visitor.visit(productType: NodeIndex(rawValue: base.rawValue))
    case SubscriptDecl.kind:
      return visitor.visit(subscript: NodeIndex(rawValue: base.rawValue))
    case SubscriptImplDecl.kind:
      return visitor.visit(subscriptImpl: NodeIndex(rawValue: base.rawValue))
    case TraitDecl.kind:
      return visitor.visit(trait: NodeIndex(rawValue: base.rawValue))
    case TypeAliasDecl.kind:
      return visitor.visit(typeAlias: NodeIndex(rawValue: base.rawValue))
    case VarDecl.kind:
      return visitor.visit(var: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
