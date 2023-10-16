import Utils

/// A function declaration synthesized during compilation.
public struct SynthesizedFunctionDecl: Hashable {

  /// The kind of a synthesized declaration.
  public enum Kind: Hashable {

    /// A deinitializer.
    case deinitialize

    /// A move-initialization method.
    case moveInitialization

    /// A move-assignment method.
    case moveAssignment

    /// A copy method.
    case copy

    /// A global initializer for a binding declaration.
    case globalInitialization(BindingDecl.ID)

  }

  /// The type of this declaration.
  public let type: LambdaType

  /// The scope in which the declaration is defined.
  public let scope: AnyScopeID

  /// The kind of the declaration.
  public let kind: Kind

  /// Creates an instance with the given properties.
  public init(_ kind: Kind, typed type: LambdaType, in scope: AnyScopeID) {
    self.kind = kind
    self.type = type
    self.scope = scope
  }

  /// The type of the declaration's receiver.
  public var receiver: AnyType {
    // Synthesized are members, so their receiver is part of their captures.
    let r = type.captures[0].type
    return RemoteType(r)?.bareType ?? r
  }

}

extension SynthesizedFunctionDecl: CustomStringConvertible {

  public var description: String {
    "(\(receiver)).\(kind)"
  }

}
