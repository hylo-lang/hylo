import Utils

/// A function declaration synthesized during compilation.
public struct SynthesizedFunctionDecl: Hashable {

  /// The kind of a synthesized declaration.
  public enum Kind: UInt8 {

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

  /// The type of the function's receiver.
  public var receiver: AnyType {
    // Synthesized functions are methods, so their first capture is the receiver.
    read(type.captures[0].type, { (r) in RemoteType(r)?.bareType ?? r })
  }

}

extension SynthesizedFunctionDecl: CustomStringConvertible {

  public var description: String {
    "(\((receiver))).\(kind)"
  }

}
