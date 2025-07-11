/// A reference to a declaration.
public enum DeclReference: Hashable, Sendable {

  /// A member reference's qualification.
  public indirect enum Receiver: Hashable, Sendable {

    /// The left operand in an infix expression, as `foo` in `foo + bar`.
    case operand

    /// Implicit, as the `.` in `.bar`; the whole name denotes a type member.
    case implicit

    /// Explicit, as `foo.` in `foo.bar` or `.foo.` in `.foo.bar`.
    case explicit(AnyExprID)

    /// Elided in a member context.
    case elided(DeclReference)

    /// Creates an instance from the qualification of a name expression.
    public init?(_ r: NameExpr.Domain) {
      switch r {
      case .none:
        return nil
      case .operand:
        self = .operand
      case .implicit:
        self = .implicit
      case .explicit(let e):
        self = .explicit(e)
      }
    }

  }

  /// A direct reference.
  case direct(AnyDeclID, GenericArguments)

  /// A reference to a member declaration bound to a receiver.
  case member(AnyDeclID, GenericArguments, Receiver)

  /// A reference to an initializer used as a constructor.
  case constructor(InitializerDecl.ID, GenericArguments)

  /// A reference to the built-in module.
  case builtinModule

  /// A reference to a built-in type.
  case builtinType

  /// A reference to a built-in function.
  case builtinFunction(BuiltinFunction)

  /// A reference to a compiler-known type.
  case compilerKnownType

  /// Converts a direct initializer reference to a constructor reference.
  public init?(constructor other: DeclReference) {
    if case .direct(let d, let a) = other, let i = InitializerDecl.ID(d) {
      self = .constructor(i, a)
    } else {
      return nil
    }
  }

  /// Accesses the referred declaration if `self` is `.direct`, `.member`, or `.constructor`.
  public var decl: AnyDeclID? {
    switch self {
    case .direct(let d, _):
      return d
    case .member(let d, _, _):
      return d
    case .constructor(let d, _):
      return AnyDeclID(d)
    default:
      return nil
    }
  }

  // The generic arguments applied to the referred declaration.
  public var arguments: GenericArguments {
    switch self {
    case .direct(_, let a):
      return a
    case .member(_, let a, _):
      return a
    case .constructor(_, let a):
      return a
    default:
      return .empty
    }
  }

  /// Returns a copy of `self` in which the generic arguments of the referred declaration and that
  /// of its receiver (if any) have been mutated by applying `modify` to `m`.
  public func modifyingArguments<M>(
    mutating m: inout M, _ modify: (inout M, GenericArguments.Value) -> GenericArguments.Value
  ) -> Self {
    switch self {
    case .direct(let d, let a):
      return .direct(d, a.mapValues({ modify(&m, $0) }))
    case .member(let d, let a, let r):
      return .member(d, a.mapValues({ modify(&m, $0) }), r)
    case .constructor(let d, let a):
      return .constructor(d, a.mapValues({ modify(&m, $0) }))
    default:
      return self
    }
  }

}

extension DeclReference.Receiver {

  /// Returns a copy of `self` in which generic arguments (if any) have been mutated by applying
  /// `modify` to `m`.
  public func modifyingArguments<M>(
    mutating m: inout M, _ modify: (inout M, GenericArguments.Value) -> GenericArguments.Value
  ) -> Self {
    switch self {
    case .elided(let r):
      return .elided(r.modifyingArguments(mutating: &m, modify))
    default:
      return self
    }
  }

}
