/// A generic size parameter declaration.
public struct GenericSizeParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.genericSizeParamDecl

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprID?

  public var name: String { identifier.value }

  public init(
    identifier: SourceRepresentable<Identifier>,
    defaultValue: AnyExprID? = nil
  ) {
    self.identifier = identifier
    self.defaultValue = defaultValue
  }

}
