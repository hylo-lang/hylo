/// A name denoting an object.
public struct NameExpr: Hashable {

  public enum Domain: Hashable {

    case none

    case implicit

    case explicit(SourceRepresentable<Expr>)

  }

  /// The domain of the name, if it is qualified.
  public var domain: Domain

  /// The stem identifier of the referred entitiy.
  public var stem: SourceRepresentable<Identifier>

  /// The argument labels of the referred entitiy.
  public var labels: [String]

  /// The operator notation of the referred entitiy.
  public var notation: OperatorNotation?

  /// The type and size arguments of the referred entity.
  public var arguments: [SourceRepresentable<GenericArgument>]

}
