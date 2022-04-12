import Utils

/// The ID of a declaration.
public protocol DeclID: NodeIDProtocol {}

extension NodeID: DeclID where T: Decl {}

/// The type-erased ID of a declaration.
public struct AnyDeclID: DeclID {

  /// The underlying type-erased ID.
  var base: AnyNodeID

  /// Creates a type-erased ID from a declaration ID.
  public init<T: DeclID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this ID, or `nil` if the type conversion failed.
  public func convert<T: Decl>(to: T.Type) -> NodeID<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case AssociatedSizeDecl.kind:
      return visitor.visit(associatedSize: NodeID(rawValue: base.rawValue))
    case AssociatedTypeDecl.kind:
      return visitor.visit(associatedType: NodeID(rawValue: base.rawValue))
    case BindingDecl.kind:
      return visitor.visit(binding: NodeID(rawValue: base.rawValue))
    case ExtensionDecl.kind:
      return visitor.visit(extension: NodeID(rawValue: base.rawValue))
    case FunDecl.kind:
      return visitor.visit(fun: NodeID(rawValue: base.rawValue))
    case GenericTypeParamDecl.kind:
      return visitor.visit(genericTypeParam: NodeID(rawValue: base.rawValue))
    case GenericSizeParamDecl.kind:
      return visitor.visit(genericSizeParam: NodeID(rawValue: base.rawValue))
    case MethodImplDecl.kind:
      return visitor.visit(methodImpl: NodeID(rawValue: base.rawValue))
    case ModuleDecl.kind:
      return visitor.visit(module: NodeID(rawValue: base.rawValue))
    case NamespaceDecl.kind:
      return visitor.visit(namespace: NodeID(rawValue: base.rawValue))
    case ParamDecl.kind:
      return visitor.visit(param: NodeID(rawValue: base.rawValue))
    case ProductTypeDecl.kind:
      return visitor.visit(productType: NodeID(rawValue: base.rawValue))
    case SubscriptDecl.kind:
      return visitor.visit(subscript: NodeID(rawValue: base.rawValue))
    case SubscriptImplDecl.kind:
      return visitor.visit(subscriptImpl: NodeID(rawValue: base.rawValue))
    case TraitDecl.kind:
      return visitor.visit(trait: NodeID(rawValue: base.rawValue))
    case TypeAliasDecl.kind:
      return visitor.visit(typeAlias: NodeID(rawValue: base.rawValue))
    case VarDecl.kind:
      return visitor.visit(var: NodeID(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
