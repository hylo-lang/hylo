/// A value injected by the compiler.
public struct PragmaLiteralExpr: Expr, Sendable {

  /// The kind of a pragma literal.
  public enum Kind: Codable, Sendable {

    /// The file in which the literal appears.
    case file

    /// The line at which the literal appears.
    case line

  }

  /// The site from which `self` was parsed.
  public let site: SourceRange

  /// The kind of the literal.
  public let kind: Kind

  /// Creates a literal with given `kind` at `site`.
  public init(_ kind: Kind, at site: SourceRange) {
    self.site = site
    self.kind = kind
  }

}
