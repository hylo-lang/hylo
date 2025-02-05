/// A name denoting an object.
public struct NameExpr: Expr, Sendable {

  /// A name's qualification.
  public enum Domain: Codable, Hashable, Sendable {

    /// Unqualified as in `bar`.
    case none

    /// The left operand in an infix expression, as `foo` in `foo + bar`.
    case operand

    /// Implicit, as the `.` in `.bar`; the whole name denotes a type member.
    case implicit

    /// Explicit, as `foo.` in `foo.bar` or `.foo.` in `.foo.bar`.
    case explicit(AnyExprID)

  }

  public let site: SourceRange

  /// The domain of the name, if it is qualified.
  public let domain: Domain

  /// The name of the referred entity.
  public let name: SourceRepresentable<Name>

  /// The type and value arguments of the referred entity.
  public let arguments: [LabeledArgument]

  /// Creates an instance with given properties.
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
