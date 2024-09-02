/// A generic parameter declaration.
public struct GenericParameterDecl: SingleEntityDecl, ConstrainedGenericTypeDecl {

  public static let constructDescription = "generic parameter declaration"

  /// The introducer of a generic parameter declaration.
  public enum Introducer: Codable {

    /// The type introducer, `type`.
    case type

    /// The value introducer, `value`
    case value

  }

  public let site: SourceRange

  /// The parameter's introducer, if any.
  public let introducer: SourceRepresentable<Introducer>?

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  ///
  /// If this list contains exactly one name referring to a type, then `self` declares a value
  /// parameter of that type. Otherwise, it declares a generic type parameter and the names of
  /// this list refer to the traits to which it must conform.
  public let conformances: [NameExpr.ID]

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    introducer: SourceRepresentable<Introducer>?,
    identifier: SourceRepresentable<Identifier>,
    conformances: [NameExpr.ID] = [],
    defaultValue: AnyExprID? = nil,
    site: SourceRange
  ) {
    self.site = site
    self.introducer = introducer
    self.identifier = identifier
    self.conformances = conformances
    self.defaultValue = defaultValue
  }

  public var baseName: String { identifier.value }

  /// `true` iff `self` is denotes a variable that ranges over types.
  public var isTypeKinded: Bool {
    introducer?.value != .value
  }

}
