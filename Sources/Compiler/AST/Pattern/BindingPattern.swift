/// A pattern that introduces new variables.
///
/// This pattern alters the semantics of its sub-pattern. Nested name patterns create new variable
/// bindings, instead or referring to existing declarations.
public struct BindingPattern: Pattern {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      case `let`

      case `var`

      case sinklet

      case sinkvar

      case `inout`

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public var range: SourceRange?

  /// The introducer of the pattern.
  public var introducer: Introducer

  /// The sub-pattern.
  public var pattern: Pattern

  /// The type annotation of the pattern, if any.
  public var annotation: TypeExpr?

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(binding: self)
  }

}
