/// A declaration synthesized during compilation.
public struct SynthesizedDecl: Hashable {

  /// The kind of a synthesized declaration.
  public enum Kind {

    /// The deinitializer of a type.
    case deinitialize

    /// The move-initialization operator of a type.
    case moveInitialization

    /// The move-assignment operator of a type.
    case moveAssignment

    /// The copy method of a type.
    case copy

  }

  /// The type of this declaration.
  public let type: AnyType

  /// The scope in which the declaration is defined.
  public let scope: AnyScopeID

  /// The kind of the declaration.
  public let kind: Kind

  /// Creates an instance with the given properties.
  public init(_ kind: Kind, typed type: AnyType, in scope: AnyScopeID) {
    self.kind = kind
    self.type = type
    self.scope = scope
  }

}
