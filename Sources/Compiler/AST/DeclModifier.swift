/// A modifier that refines the semantics of a declaration.
public struct DeclModifier {

  /// The value of a declaration modifier.
  public enum Kind {

    /// The declaration is public.
    case pub

    /// The declaration is only visible within its module.
    case mod

    /// On a method declaration, the modifier indicates that `self` is consuming. On a parameter
    /// declaration, the modifier indicates that the parameter is consuming.
    case consuming

    /// On a method declaration, the modifier indicates that `self` is mutating. On a parameter
    /// declaration, the modifier indicates that the parameter is mutating.
    case mut

    /// The declaration denotes a function that might be called as an infix operator.
    case infix

    /// The declaration denotes a function that might be called as an prefix operator.
    case prefix

    /// The declaration denotes a function that might be called as an postfix operator.
    case postfix

    /// The declaration denotes a closure that captures a mutable state.
    case volatile

    /// The declaration is a member of a type itself, rather than instances of the type. That
    /// modifier is valid only on method and property declarations.
    case `static`

  }

  /// The value of the modifier.
  public var kind: Kind

  /// The source range of this modifier's textual representation.
  public var range: SourceRange?

  public init(kind: Kind, range: SourceRange? = nil) {
    self.kind = kind
    self.range = range
  }

}
