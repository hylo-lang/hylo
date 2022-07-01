/// A reference to a declaration.
public enum DeclRef {

  /// A direct reference.
  case direct(AnyDeclID)

  /// A reference to a member declaration bound to `self`.
  case member(AnyDeclID)

  /// Accesses the referred declaration.
  public var decl: AnyDeclID {
    switch self {
    case .direct(let d): return d
    case .member(let d): return d
    }
  }

}
