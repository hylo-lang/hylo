/// A parameter declaration in a function or subscript declaration.
public struct ParamDecl: SingleEntityDecl {

  /// The label of the parameter.
  public var label: SourceRepresentable<Identifier>

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The type annotation of the declaration, if any.
  public var annotation: AnyTypeExprIndex?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprIndex?

  public var name: String { identifier.value }

}
