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

  public var typeID: ObjectIdentifier { base.typeID }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Decl>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch base.typeID {
    case AssociatedTypeDecl.typeID:
      return visitor.visit(associatedType: NodeIndex(rawValue: base.rawValue))
    case BindingDecl.typeID:
      return visitor.visit(binding: NodeIndex(rawValue: base.rawValue))
    case ExtensionDecl.typeID:
      return visitor.visit(extension: NodeIndex(rawValue: base.rawValue))
    case FunDecl.typeID:
      return visitor.visit(fun: NodeIndex(rawValue: base.rawValue))
    case GenericTypeParamDecl.typeID:
      return visitor.visit(genericTypeParam: NodeIndex(rawValue: base.rawValue))
    case GenericSizeParamDecl.typeID:
      return visitor.visit(genericSizeParam: NodeIndex(rawValue: base.rawValue))
    case MethodImplDecl.typeID:
      return visitor.visit(methodImpl: NodeIndex(rawValue: base.rawValue))
    case ModuleDecl.typeID:
      return visitor.visit(module: NodeIndex(rawValue: base.rawValue))
    case NamespaceDecl.typeID:
      return visitor.visit(namespace: NodeIndex(rawValue: base.rawValue))
    case ParamDecl.typeID:
      return visitor.visit(param: NodeIndex(rawValue: base.rawValue))
    case ProductTypeDecl.typeID:
      return visitor.visit(productType: NodeIndex(rawValue: base.rawValue))
    case SubscriptDecl.typeID:
      return visitor.visit(subscript: NodeIndex(rawValue: base.rawValue))
    case SubscriptImplDecl.typeID:
      return visitor.visit(subscriptImpl: NodeIndex(rawValue: base.rawValue))
    case TraitDecl.typeID:
      return visitor.visit(trait: NodeIndex(rawValue: base.rawValue))
    case TypeAliasDecl.typeID:
      return visitor.visit(typeAlias: NodeIndex(rawValue: base.rawValue))
    case VarDecl.typeID:
      return visitor.visit(var: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
