/// A name denoting an object.
public struct NameExpr: Expr {

  public static let kind = NodeKind.nameExpr

  public enum Domain: Hashable {

    case none

    case implicit

    case explicit(AnyExprID)

  }

  /// The domain of the name, if it is qualified.
  public var domain: Domain

  /// The stem identifier of the referred entitiy.
  public var stem: SourceRepresentable<Identifier>

  /// The argument labels of the referred entitiy.
  public var labels: [String?]

  /// The operator notation of the referred entitiy.
  public var notation: OperatorNotation?

  /// The type and size arguments of the referred entity.
  public var arguments: [SourceRepresentable<GenericArgument>]

  /// Creates a new name expression.
  public init(
    domain: Domain = .none,
    stem: SourceRepresentable<Identifier>,
    labels: [String?] = [],
    arguments: [SourceRepresentable<GenericArgument>] = []
  ) {
    self.domain = domain
    self.stem = stem
    self.labels = labels
    self.notation = nil
    self.arguments = arguments
  }

  /// Creates a new operator name expression.
  public init(
    domain: Domain = .none,
    stem: SourceRepresentable<Identifier>,
    notation: OperatorNotation,
    arguments: [SourceRepresentable<GenericArgument>] = []
  ) {
    self.domain = domain
    self.stem = stem
    self.labels = []
    self.notation = notation
    self.arguments = arguments
  }

  /// A string representation of the base name denoted by this expression.
  public var baseName: String {
    if let notation = notation {
      return "\(notation)\(stem.value)"
    } else if labels.isEmpty {
      return stem.value
    } else {
      let labels = labels.reduce(into: "", { (s, l) in s += (l ?? "_") + ":" })
      return "\(stem.value)(\(labels))"
    }
  }

}
