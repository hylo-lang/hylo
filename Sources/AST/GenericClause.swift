/// The generic clause of a generic declaration.
///
/// This includes generic parameters, as well as the constraints by a where clause.
public struct GenericClause {

  public init(params: [GenericParamType], reqs: [TypeReq]) {
    self.params = params
    self.reqs = reqs
  }

  /// The generic parameter types of the signature.
  public var params: [GenericParamType]

  /// The type requirements in the clause.
  public var reqs: [TypeReq]

}
