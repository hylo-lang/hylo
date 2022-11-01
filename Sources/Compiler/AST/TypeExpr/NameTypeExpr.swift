/// A name denoting a nominal type.
public struct NameTypeExpr: TypeExpr {

  public static let kind = NodeKind.nameTypeExpr

  /// The domain of the name, if it is qualified.
  public var domain: AnyTypeExprID?

  /// The identifier of the referred type.
  public let identifier: SourceRepresentable<Identifier>

  /// The type and value arguments of the referred type.
  public let arguments: [GenericArgument]

  public init(
    domain: AnyTypeExprID? = nil,
    identifier: SourceRepresentable<Identifier>,
    arguments: [GenericArgument]
  ) {
    self.domain = domain
    self.identifier = identifier
    self.arguments = arguments
  }

  /// Incorporates `domain` into `self`.
  ///
  /// - Precondition: `self.domain == nil`
  internal mutating func incorporate(domain: AnyTypeExprID) {
    precondition(self.domain == nil)
    self.domain = domain
  }
}
