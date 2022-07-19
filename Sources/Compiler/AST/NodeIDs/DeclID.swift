import Utils

/// The ID of a declaration.
public protocol DeclID: NodeIDProtocol {}

extension DeclID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .associatedTypeDecl:
      return visitor.visit(associatedType: NodeID(unsafeRawValue: rawValue))
    case .associatedValueDecl:
      return visitor.visit(associatedValue: NodeID(unsafeRawValue: rawValue))
    case .bindingDecl:
      return visitor.visit(binding: NodeID(unsafeRawValue: rawValue))
    case .extensionDecl:
      return visitor.visit(extension: NodeID(unsafeRawValue: rawValue))
    case .funDecl:
      return visitor.visit(fun: NodeID(unsafeRawValue: rawValue))
    case .genericTypeParamDecl:
      return visitor.visit(genericTypeParam: NodeID(unsafeRawValue: rawValue))
    case .genericValueParamDecl:
      return visitor.visit(genericValueParam: NodeID(unsafeRawValue: rawValue))
    case .methodImplDecl:
      return visitor.visit(methodImpl: NodeID(unsafeRawValue: rawValue))
    case .moduleDecl:
      return visitor.visit(module: NodeID(unsafeRawValue: rawValue))
    case .namespaceDecl:
      return visitor.visit(namespace: NodeID(unsafeRawValue: rawValue))
    case .operatorDecl:
      return visitor.visit(operator: NodeID(unsafeRawValue: rawValue))
    case .parameterDecl:
      return visitor.visit(param: NodeID(unsafeRawValue: rawValue))
    case .productTypeDecl:
      return visitor.visit(productType: NodeID(unsafeRawValue: rawValue))
    case .subscriptDecl:
      return visitor.visit(subscript: NodeID(unsafeRawValue: rawValue))
    case .subscriptImplDecl:
      return visitor.visit(subscriptImpl: NodeID(unsafeRawValue: rawValue))
    case .traitDecl:
      return visitor.visit(trait: NodeID(unsafeRawValue: rawValue))
    case .typeAliasDecl:
      return visitor.visit(typeAlias: NodeID(unsafeRawValue: rawValue))
    case .varDecl:
      return visitor.visit(var: NodeID(unsafeRawValue: rawValue))
    default:
      unreachable()
    }
  }

}

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

}
