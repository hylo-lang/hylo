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

    /// Lambda generated for an autoclosure argument.
    case autoclosure

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

  /// Returns the generic parameters of the declaration.
  public var genericParameters: [GenericParameterDecl.ID] {
    guard !type.captures.isEmpty else { return [] }
    guard let t = BoundGenericType(receiver) else { return [] }
    return t.arguments.compactMap { (k, v) -> GenericParameterDecl.ID? in
      if let u = v as? AnyType {
        return GenericTypeParameterType(u)?.decl == k ? k : nil
      } else {
        UNIMPLEMENTED("compile time values")
      }
    }
  }

}

extension SynthesizedFunctionDecl: CustomStringConvertible {

  public var description: String {
    if type.captures.isEmpty {
      return "\(kind)"
    } else {
      return "(\(receiver)).\(kind)"
    }
  }

}
