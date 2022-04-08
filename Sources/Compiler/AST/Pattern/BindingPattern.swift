/// A pattern that introduces new variables.
///
/// This pattern alters the semantics of its sub-pattern. Nested name patterns create new variable
/// bindings, instead or referring to existing declarations.
public struct BindingPattern: Hashable {

  public enum Introducer: Hashable {

    case `let`

    case `var`

    case sinklet

    case sinkvar

    case `inout`

  }

  /// The introducer of the pattern.
  public var introducer: SourceRepresentable<Introducer>

  /// The sub-pattern.
  public var subpattern: SourceRepresentable<Pattern>

  /// The type annotation of the pattern, if any.
  public var annotation: SourceRepresentable<TypeExpr>?

}
