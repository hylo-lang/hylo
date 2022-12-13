/// A name denoting an object.
public struct NameExpr: Expr {

  /// A name's qualification
  ///
  /// This type is notionaly equivalent to `Optional<D>` where `D` is an enum with the cases
  /// `implicit` and `expr(AnyExprID)`.
  public enum Domain: ExpressibleByNilLiteral, Equatable, Codable {

    /// Domain is absent, as in `bar`.
    case none

    /// Domain is implicit, as in `.bar`; the expression denotes a type member.
    case implicit

    /// Domain is an expression, as in `foo.bar`.
    case expr(AnyExprID)

    public init(nilLiteral: ()) {
      self = .none
    }

  }

  public let origin: SourceRange?

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
    origin: SourceRange?
  ) {
    self.origin = origin
    self.domain = domain
    self.name = name
    self.arguments = arguments
  }

}
