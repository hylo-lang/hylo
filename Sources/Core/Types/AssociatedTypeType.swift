import Utils

/// An associated type of a generic type parameter, or associated type thereof.
public struct AssociatedTypeType: TypeProtocol {

  /// The declaration that introduces the associated type in the parent trait.
  public let decl: AssociatedTypeDecl.ID

  /// The type whose `self` is member.
  ///
  /// `domain` is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: AnyType

  /// The name of the associated type.
  public let name: Incidental<String>

  /// A set of flags describing recursive properties.
  public let flags: TypeFlags

  /// Creates an instance denoting the associated type declared by `decl` as a member of `domain`.
  public init(_ decl: AssociatedTypeDecl.ID, domain: AnyType, ast: AST) {
    var fs = domain.flags
    if !domain.isSkolem && !(domain.base is TypeVariable) {
      fs.remove(.isCanonical)
    }

    self.domain = domain
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
    self.flags = fs
  }


}

extension AssociatedTypeType: CustomStringConvertible {

  public var description: String {
    if domain.base is ConformanceLensType {
      return "(\(domain)).\(name.value)"
    } else {
      return "\(domain).\(name.value)"
    }
  }

}
