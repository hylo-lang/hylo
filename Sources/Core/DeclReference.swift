/// A reference to a declaration.
public enum DeclReference: Hashable {

  /// A direct reference.
  case direct(AnyDeclID)

  /// A reference to a member declaration bound to a receiver.
  case member(AnyDeclID)

  /// A reference to an initializer used as a constructor.
  case constructor(InitializerDecl.ID)

  /// A reference to a built-in function.
  case builtinFunction(BuiltinFunction)

  /// A reference to a built-in type.
  case builtinType

  /// Converts a direct initializer reference to a constructor reference.
  public init?(constructor other: DeclReference) {
    if case .direct(let d) = other, let i = InitializerDecl.ID(d) {
      self = .constructor(i)
    } else {
      return nil
    }
  }

  /// Accesses the referred declaration if `self` is `.direct`, `.member`, or `.constructor`.
  public var decl: AnyDeclID? {
    switch self {
    case .direct(let d):
      return d
    case .member(let d):
      return d
    case .constructor(let d):
      return AnyDeclID(d)
    default:
      return nil
    }
  }

}
