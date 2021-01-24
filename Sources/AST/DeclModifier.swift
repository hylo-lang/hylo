import Basic

/// A modifier that refines the semantics of a declaration.
public struct DeclModifier {

  public init(kind: Kind, range: SourceRange) {
    self.kind = kind
    self.range = range
  }

  /// The value of the modifier.
  public var kind: Kind

  /// The source range of this modifier's textual representation.
  public var range: SourceRange

  /// The value of a declaration modifier.
  public enum Kind {

    /// A modifier indicating that a method expects `self` to be mutating.
    case mut

    /// A modifier indicating that a method or variable is a member of the type itself, rather than
    /// instances of the type.
    case `static`

    /// A type modifier indicating that instances of the type cannot be copied.
    case moveonly

  }

}
