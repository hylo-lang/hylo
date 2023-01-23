/// A generic parameter declaration.
public struct GenericParameterDecl: SingleEntityDecl {

  public let origin: SourceRange

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  ///
  /// If this list contains exactly one name referring to a type, then `self` declares a value
  /// parameter of that type. Otherwise, it declares a generic type parameter and the names of
  /// this list refer to the traits to which it must conform.
  public let conformances: [NodeID<NameExpr>]

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    conformances: [NodeID<NameExpr>] = [],
    defaultValue: AnyExprID? = nil,
    origin: SourceRange
  ) {
    self.origin = origin
    self.identifier = identifier
    self.conformances = conformances
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
