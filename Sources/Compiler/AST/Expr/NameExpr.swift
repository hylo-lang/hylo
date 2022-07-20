/// A name denoting an object.
public struct NameExpr: Expr {

  public static let kind = NodeKind.nameExpr

  public enum Domain: Hashable {

    case none

    case implicit

    case explicit(AnyExprID)

  }

  /// The domain of the name, if it is qualified.
  public var domain: Domain

  /// The name of the referred entity.
  public var name: SourceRepresentable<Name>

  /// The type and value arguments of the referred entity.
  public var arguments: [GenericArgument]

  public init(
    domain: Domain = .none,
    name: SourceRepresentable<Name>,
    arguments: [GenericArgument] = []
  ) {
    self.domain = domain
    self.name = name
    self.arguments = arguments
  }

}
