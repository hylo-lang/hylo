import Utils

/// The ID of a declaration.
public protocol DeclID: NodeIDProtocol {}

extension DeclID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .associatedTypeDecl:
      return visitor.visit(associatedType: NodeID(rawValue: rawValue))
    case .associatedValueDecl:
      return visitor.visit(associatedValue: NodeID(rawValue: rawValue))
    case .bindingDecl:
      return visitor.visit(binding: NodeID(rawValue: rawValue))
    case .conformanceDecl:
      return visitor.visit(conformance: NodeID(rawValue: rawValue))
    case .extensionDecl:
      return visitor.visit(extension: NodeID(rawValue: rawValue))
    case .funDecl:
      return visitor.visit(fun: NodeID(rawValue: rawValue))
    case .genericTypeParamDecl:
      return visitor.visit(genericTypeParam: NodeID(rawValue: rawValue))
    case .genericValueParamDecl:
      return visitor.visit(genericValueParam: NodeID(rawValue: rawValue))
    case .importDecl:
      return visitor.visit(import: NodeID(rawValue: rawValue))
    case .methodImplDecl:
      return visitor.visit(methodImpl: NodeID(rawValue: rawValue))
    case .moduleDecl:
      return visitor.visit(module: NodeID(rawValue: rawValue))
    case .namespaceDecl:
      return visitor.visit(namespace: NodeID(rawValue: rawValue))
    case .operatorDecl:
      return visitor.visit(operator: NodeID(rawValue: rawValue))
    case .parameterDecl:
      return visitor.visit(param: NodeID(rawValue: rawValue))
    case .productTypeDecl:
      return visitor.visit(productType: NodeID(rawValue: rawValue))
    case .subscriptDecl:
      return visitor.visit(subscript: NodeID(rawValue: rawValue))
    case .subscriptImplDecl:
      return visitor.visit(subscriptImpl: NodeID(rawValue: rawValue))
    case .traitDecl:
      return visitor.visit(trait: NodeID(rawValue: rawValue))
    case .typeAliasDecl:
      return visitor.visit(typeAlias: NodeID(rawValue: rawValue))
    case .varDecl:
      return visitor.visit(var: NodeID(rawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: DeclID where Subject: Decl {}

/// The type-erased ID of a declaration.
public struct AnyDeclID: DeclID {

  /// The underlying type-erased ID.
  private var base: AnyNodeID

  /// Creates a type-erased ID from a declaration ID.
  public init<T: DeclID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
