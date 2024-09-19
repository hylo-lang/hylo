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
  public let flags: ValueFlags

  /// Creates an instance denoting the associated type declared by `decl` as a member of `domain`.
  public init(_ decl: AssociatedTypeDecl.ID, domain: AnyType, ast: AST) {
    self.init(decl: decl, domain: domain, name: ast[decl].baseName)
  }

  /// Creates an instance with the given properties.
  init(decl: AssociatedTypeDecl.ID, domain: AnyType, name: String) {
    var fs = domain.flags

    let d = AssociatedTypeType(domain)?.root ?? domain
    if !(d.isSkolem || d.base is TypeVariable) {
      fs.insert(.hasNonCanonical)
    }

    self.decl = decl
    self.domain = domain
    self.name = Incidental(name)
    self.flags = fs
  }

  /// Returns the root of `self`'s qualification.
  public var root: AnyType {
    AssociatedTypeType(domain)?.root ?? domain
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    let d = domain.transform(mutating: &m, transformer)
    return AssociatedTypeType(decl: decl, domain: d, name: name.value)
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
