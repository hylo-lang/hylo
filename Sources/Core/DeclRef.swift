/// A reference to a declaration.
public enum DeclRef: Hashable {

  /// A direct reference.
  case direct(AnyDeclID)

  /// A reference to a declaration bound to a receiver.
  case member(AnyDeclID)

  /// A reference to a built-in function.
  case builtinFunction(BuiltinFunction)

  /// A reference to a built-in type.
  case builtinType

  /// Accesses the referred declaration if `self` is `.direct` or `.member`.
  public var decl: AnyDeclID? {
    switch self {
    case .direct(let d):
      return d
    case .member(let d):
      return d
    default:
      return nil
    }
  }

}
