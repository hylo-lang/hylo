/// A generic type parameter declaration.
public struct GenericTypeParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.genericTypeParamDecl

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public let conformances: [NodeID<NameTypeExpr>]

  /// The default value of the declaration, if any.
  public let defaultValue: AnyTypeExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    conformances: [NodeID<NameTypeExpr>] = [],
    defaultValue: AnyTypeExprID? = nil
  ) {
    self.identifier = identifier
    self.conformances = conformances
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
