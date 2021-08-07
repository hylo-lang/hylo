import Basic

/// A modifier that refines the semantics of a declaration.
public struct DeclModifier {

  /// The value of a declaration modifier.
  public enum Kind {

    /// The declaration is public.
    case pub

    /// The declaration is only visible within its module.
    case mod

    /// The declaration expects `self` to be mutating. That modifier is valid only on methods.
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

    /// The declaration denotes a type whose instances cannot be copied.
    case moveonly

  }

  public init(kind: Kind, range: SourceRange) {
    self.kind = kind
    self.range = range
  }

  /// The value of the modifier.
  public var kind: Kind

  /// The source range of this modifier's textual representation.
  public var range: SourceRange

}
