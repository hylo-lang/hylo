/// An unqualified name denoting an entity.
public struct Name: Hashable, Codable {

  /// The stem identifier of the referred entity.
  public let stem: Identifier

  /// The argument labels of the referred entity, given that it is a function.
  public let labels: [String?]

  /// The operator notation of the referred entity, given that it is an operator.
  public let notation: OperatorNotation?

  /// The method introducer of the referred entity, given that it is a method implementation.
  ///
  /// The introducer is incorporated during parsing, after the instance is created.
  public private(set) var introducer: ImplIntroducer? = nil

  /// Creates a new name.
  public init(
    stem: Identifier,
    labels: [String?] = []
  ) {
    self.stem = stem
    self.labels = labels
    self.notation = nil
  }

  /// Creates a new operator name.
  public init(
    stem: Identifier,
    notation: OperatorNotation
  ) {
    self.stem = stem
    self.labels = []
    self.notation = notation
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

// MARK: Incorporation of introducers

extension Name {
  /// Returns `self` with `introducer` incorporated into its value.
  ///
  /// - Precondition: `self` has no introducer.`
  internal func introduced(by newIntroducer: ImplIntroducer) -> Self {
    precondition(self.introducer == nil)
    var r = self
    r.introducer = newIntroducer
    return r
  }
}

extension SourceRepresentable where Part == Name {
  /// Returns `self` with `introducer` incorporated into its value.
  ///
  /// - Precondition: `self` has no introducer.`
  internal func introduced(by introducer: SourceRepresentable<ImplIntroducer>) -> Self {
    return .init(
      value: self.value.introduced(by: introducer.value),
      range: self.range!.upperBounded(by: introducer.range!.upperBound))
  }
}
