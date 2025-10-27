import Utils

/// A pattern that introduces new variables.
///
/// This pattern alters the semantics of its sub-pattern. Nested name patterns create new variable
/// bindings, instead of referring to existing declarations.
public struct BindingPattern: Pattern {

  /// An introducer in a binding pattern.
  public enum Introducer: Codable, Sendable {

    case `let`

    case `var`

    case sinklet

    case `inout`

    /// `true` if `self` is `var` or `sinklet`.
    public var isConsuming: Bool {
      (self == .var) || (self == .sinklet)
    }

  }

  public let site: SourceRange

  /// The introducer of the pattern.
  public let introducer: SourceRepresentable<Introducer>

  /// The sub-pattern.
  ///
  /// - Requires: `subpattern` may not contain other binding patterns.
  public let subpattern: AnyPatternID

  /// The type annotation of the pattern, if any.
  public let annotation: AnyExprID?

  public init(
    introducer: SourceRepresentable<BindingPattern.Introducer>,
    subpattern: AnyPatternID,
    annotation: AnyExprID?,
    site: SourceRange
  ) {
    self.site = site
    self.introducer = introducer
    self.subpattern = subpattern
    self.annotation = annotation
  }

}
