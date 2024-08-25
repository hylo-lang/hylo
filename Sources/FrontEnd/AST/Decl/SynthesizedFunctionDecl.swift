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

    /// An equality method.
    case equal

    /// A global initializer for a binding declaration.
    case globalInitialization(BindingDecl.ID)

    /// Lambda generated for an autoclosure argument.
    case autoclosure(AnyExprID)

  }

  /// The type of this declaration.
  public let type: ArrowType

  /// The generic parameters of the declaration.
  public let genericParameters: [GenericParameterDecl.ID]

  /// The scope in which the declaration is defined.
  public let scope: AnyScopeID

  /// The kind of the declaration.
  public let kind: Kind

  /// Creates an instance with the given properties.
  ///
  /// - Requires: The environment of `type` is a tuple.
  public init(
    _ kind: Kind,
    typed type: ArrowType,
    parameterizedBy genericParameters: [GenericParameterDecl.ID],
    in scope: AnyScopeID
  ) {
    precondition(type.isCanonical)
    precondition(type.environment.base is TupleType)
    self.type = type
    self.genericParameters = genericParameters
    self.scope = scope
    self.kind = kind
  }

  /// The type of the declaration's receiver.
  ///
  /// - Requires: `self` is a synthetic implementation of a member function.
  public var receiver: AnyType {
    read(type.captures[0].type, { RemoteType($0)?.bareType ?? $0 })
  }

}

extension SynthesizedFunctionDecl: CustomStringConvertible {

  public var description: String {
    type.captures.isEmpty ? "\(scope).\(kind)" : "\(scope).(\(receiver)).\(kind)"
  }

}
