/// The generic signature of a generic declaration.
///
/// This includes generic parameters, as well as the constraints by a where clause.
public struct GenericSignature {

  /// The generic parameter types of the signature.
  public let genericParams: [GenericParamType]

}
