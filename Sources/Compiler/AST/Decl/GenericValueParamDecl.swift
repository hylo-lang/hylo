/// A generic value parameter declaration.
public struct GenericValueParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.genericValueParamDecl

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The type annotation of the pattern.
  public let annotation: AnyTypeExprID

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    annotation: AnyTypeExprID,
    defaultValue: AnyExprID? = nil
  ) {
    self.identifier = identifier
    self.annotation = annotation
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
