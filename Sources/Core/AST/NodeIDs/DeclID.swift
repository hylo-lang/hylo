import Utils

/// The ID of a declaration.
public protocol DeclID: NodeIDProtocol {}

extension DeclID {

  /// Indicates whether `self` denotes an overloadable declaration.
  public var isOverloadable: Bool {
    switch kind {
    case FunctionDecl.self, InitializerDecl.self, MethodDecl.self, SubscriptDecl.self:
      return true
    default:
      return false
    }
  }

  /// `true` iff `self` denotes an associated type or value.
  public var isAssociatedDecl: Bool {
    (kind == AssociatedTypeDecl.self) || (kind == AssociatedValueDecl.self)
  }

  /// `true` iff `self` denotes the declaration of a callable entity.
  public var isCallable: Bool {
    (kind.value as! Decl.Type).isCallable
  }

  /// `true` iff `self` denotes a conformance source.
  public var isConformanceSource: Bool {
    kind.value is ConformanceSource.Type
  }

}

extension NodeID: DeclID where Subject: Decl {}

/// The type-erased ID of a declaration.
public struct AnyDeclID: DeclID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from a declaration ID.
  public init<T: DeclID>(_ other: T) {
    self.base = AnyNodeID(other)
  }

  /// Creates an instance with the same raw value as `x` failing iff `!(x.kind is Decl)`.
  public init?<T: NodeIDProtocol>(_ x: T) {
    if x.kind.value is Decl.Type {
      self.base = AnyNodeID(x)
    } else {
      return nil
    }
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
