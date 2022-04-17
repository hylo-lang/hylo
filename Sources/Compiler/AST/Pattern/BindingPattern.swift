/// A pattern that introduces new variables.
///
/// This pattern alters the semantics of its sub-pattern. Nested name patterns create new variable
/// bindings, instead of referring to existing declarations.
public struct BindingPattern: Pattern {

  public static let kind = NodeKind.bindingPattern

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
  ///
  /// - Requires: `subpattern` may not contain other binding patterns.
  public var subpattern: AnyPatternID

  /// The type annotation of the pattern, if any.
  public var annotation: AnyTypeExprID?

}
