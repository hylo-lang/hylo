/// A name denoting an object.
public struct NameExpr: Expr {

  /// A name's qualification
  public enum Domain: Equatable, Codable {

    /// Unqualified as in `bar`.
    case none

    /// Implicit as the `.` in `.bar`; the whole name denotes a type member.
    case implicit

    /// Explicit, as `foo.` in `foo.bar` or `.foo.` in `.foo.bar`.
    case expr(AnyExprID)

  }

  public let site: SourceRange

  /// The domain of the name, if it is qualified.
  public let domain: Domain

  /// The name of the referred entity.
  public let name: SourceRepresentable<Name>

  /// The type and value arguments of the referred entity.
  public let arguments: [LabeledArgument]

  public init(
    domain: Domain = .none,
    name: SourceRepresentable<Name>,
    arguments: [LabeledArgument] = [],
    site: SourceRange
  ) {
    self.site = site
    self.domain = domain
    self.name = name
    self.arguments = arguments
  }

}
