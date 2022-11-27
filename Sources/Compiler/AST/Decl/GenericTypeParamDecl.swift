/// A generic type parameter declaration.
public struct GenericTypeParamDecl: TypeDecl {

  public let origin: SourceRange?

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public let conformances: [NodeID<NameExpr>]

  /// The default value of the declaration, if any.
  public let defaultValue: AnyTypeExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    conformances: [NodeID<NameExpr>] = [],
    defaultValue: AnyTypeExprID? = nil,
    origin: SourceRange?
  ) {
    self.origin = origin
    self.identifier = identifier
    self.conformances = conformances
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
