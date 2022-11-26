/// A name denoting an object.
public struct NameExpr: Expr {

  public enum Domain: Equatable, Codable {

    /// No domain.
    case none

    /// Domain is implicit; the expression denotes a type member.
    case implicit

    /// Domain is a value expression or a type identifier.
    case expr(AnyExprID)

    /// Domain is a type typression.
    case type(AnyTypeExprID)

  }

  public let origin: SourceRange?

  /// The domain of the name, if it is qualified.
  public let domain: Domain

  /// The name of the referred entity.
  public let name: SourceRepresentable<Name>

  /// The type and value arguments of the referred entity.
  public let arguments: [GenericArgument]

  public init(
    domain: Domain = .none,
    name: SourceRepresentable<Name>,
    arguments: [GenericArgument] = [],
    origin: SourceRange?
  ) {
    self.origin = origin
    self.domain = domain
    self.name = name
    self.arguments = arguments
  }

}
