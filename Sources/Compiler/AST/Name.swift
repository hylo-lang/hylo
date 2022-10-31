/// An unqualified name denoting an entity.
public struct Name: Hashable, Codable {

  /// The stem identifier of the referred entity.
  public let stem: Identifier

  /// The argument labels of the referred entity, given that it is a function.
  public let labels: [String?]

  /// The operator notation of the referred entity, given that it is an operator.
  public let notation: OperatorNotation?

  /// The method introducer of the referred entity, given that it is a method implementation.
  public var introducer: ImplIntroducer?

  /// Creates a new name.
  public init(
    stem: Identifier,
    labels: [String?] = [],
    introducer: ImplIntroducer? = nil
  ) {
    self.stem = stem
    self.labels = labels
    self.notation = nil
    self.introducer = introducer
  }

  /// Creates a new operator name.
  public init(
    stem: Identifier,
    notation: OperatorNotation,
    introducer: ImplIntroducer? = nil
  ) {
    self.stem = stem
    self.labels = []
    self.notation = notation
    self.introducer = introducer
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init?(ofFunction decl: NodeID<FunDecl>, in ast: AST) {
    guard let stem = ast[decl].identifier?.value else { return nil }
    if let notation = ast[decl].notation?.value {
      self.init(stem: stem, notation: notation)
    } else {
      self.init(stem: stem, labels: ast[decl].parameters.map({ ast[$0].label?.value }))
    }
  }

}

extension Name: CustomStringConvertible {

  public var description: String {
    if let notation = notation {
      return "\(notation)\(stem)"
    } else if labels.isEmpty {
      let introducer = introducer.map({ ".\($0)" }) ?? ""
      return stem + introducer
    } else {
      let labels = labels.reduce(into: "", { (s, l) in s += (l ?? "_") + ":" })
      let introducer = introducer.map({ ".\($0)" }) ?? ""
      return "\(stem)(\(labels))" + introducer
    }
  }

}

extension Name: ExpressibleByStringLiteral {

  public init(stringLiteral value: StringLiteralType) {
    self.init(stem: value)
  }

}
