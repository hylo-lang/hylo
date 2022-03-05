/// The generic clause of a generic declaration.
///
/// This includes generic parameters and their type requirements.
public class GenericClause: Node {

  /// The source range of this clause's textual representation.
  public var range: SourceRange?

  /// The generic parameter types of the signature.
  public var params: [GenericParamDecl]

  /// The type requirements in the clause.
  public var typeReqs: [TypeReq]

  public init(params: [GenericParamDecl], typeReqs: [TypeReq]) {
    self.params = params
    self.typeReqs = typeReqs
  }

}

/// A type requirement in a generic clause.
public struct TypeReq {

  /// The kind of a type requirement.
  public enum Kind {

    /// A requirement `T == U` prescribing that the generic parameter `T` be equal to the type `U`.
    case equality

    /// A requirement `T: V` prescribing that the generic parameter `T` conform to the view `V`.
    case conformance

  }

  /// The kind of the requirement.
  public var kind: Kind

  /// The left operand of the requirement.
  public var lhs: NameSign

  /// The right operand of the requirement.
  public var rhs: Sign

  /// The source range of this requirement's textual representation.
  public var range: SourceRange?

  public init(kind: Kind, lhs: NameSign, rhs: Sign, range: SourceRange? = nil) {
    self.kind = kind
    self.lhs = lhs
    self.rhs = rhs
    self.range = range
  }

}
