/// A name denoting an object.
public struct NameExpr: Expr {

  public enum Domain {

    case none

    case implicit

    case explicit(Expr)

  }

  public var range: SourceRange?

  /// The domain of the name, if it is qualified.
  public var domain: Domain

  /// The stem identifier of the referred entitiy.
  public var stem: Identifier

  /// The argument labels of the referred entitiy.
  public var labels: [String]

  /// The operator notation of the referred entitiy.
  public var notation: OperatorNotation?

  /// The type and size arguments of the referred entity.
  public var arguments: [GenericArgument]

}
