import Basic

/// The generic clause of a generic declaration.
///
/// This includes generic parameters and their type requirements.
public class GenericClause: Node {

  public init(params: [GenericParamDecl], typeReqs: [TypeReq], range: SourceRange) {
    self.params = params
    self.typeReqs = typeReqs
    self.range = range
  }

  /// The generic parameter types of the signature.
  public var params: [GenericParamDecl]

  /// The type requirements in the clause.
  public var typeReqs: [TypeReq]

  /// The source range of this clause's textual representation.
  public var range: SourceRange

}

/// A type requirement in a generic clause.
public struct TypeReq {

  public init(kind: Kind, lhs: IdentTypeRepr, rhs: TypeRepr, range: SourceRange) {
    self.kind = kind
    self.lhs = lhs
    self.rhs = rhs
    self.range = range
  }

  /// The kind of the requirement.
  public var kind: Kind

  /// The left operand of the requirement.
  public var lhs: IdentTypeRepr

  /// The right operand of the requirement.
  public var rhs: TypeRepr

  /// The source range of this requirement's textual representation.
  public var range: SourceRange

  /// The kind of a type requirement.
  public enum Kind {

    /// A requirement `T == U` prescribing that the generic parameter `T` be equal to the type `U`.
    case equality

    /// A requirement `T: V` prescribing that the generic parameter `T` conform to the view `V`.
    case conformance

  }

}
