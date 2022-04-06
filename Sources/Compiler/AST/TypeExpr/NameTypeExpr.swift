import Utils

/// A name denoting a nominal type.
public struct NameTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The domain of the name, if it is qualified.
  public var domain: TypeExpr?

  /// The identifier of the referred type.
  public var identifier: Identifier

  /// The type and size arguments of the referred type.
  public var arguments: [GenericArgument]

}

extension NameTypeExpr: CustomStringConvertible {

  public var description: String {
    let domain = self.domain.map({ "\($0)." }) ?? ""
    let arguments = self.arguments.isEmpty
      ? ""
      : "<" + String.joining(arguments, separator: ", ") + ">"
    return "\(domain).\(identifier)\(arguments)"
  }

}
