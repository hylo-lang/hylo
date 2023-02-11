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
  /// The introducer, if any, is incorporated during parsing, after the original `Name` is created.
  public let introducer: ImplIntroducer?

  /// Creates a new name.
  public init(
    stem: Identifier,
    labels: [String?] = []
  ) {
    self.stem = stem
    self.labels = labels
    self.notation = nil
    self.introducer = nil
  }

  /// Creates a new operator name.
  public init(
    stem: Identifier,
    notation: OperatorNotation
  ) {
    self.stem = stem
    self.labels = []
    self.notation = notation
    self.introducer = nil
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init?(of decl: NodeID<FunctionDecl>, in ast: AST) {
    guard let stem = ast[decl].identifier?.value else { return nil }
    if let notation = ast[decl].notation?.value {
      self.init(stem: stem, notation: notation)
    } else {
      self.init(stem: stem, labels: ast[ast[decl].parameters].map(\.label?.value))
    }
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init(of decl: NodeID<MethodDecl>, in ast: AST) {
    let stem = ast[decl].identifier.value
    if let notation = ast[decl].notation?.value {
      self.init(stem: stem, notation: notation)
    } else {
      self.init(stem: stem, labels: ast[ast[decl].parameters].map(\.label?.value))
    }
  }

  /// Creates an instance with the given properties.
  public init(
    stem: Identifier,
    labels: [String?],
    notation: OperatorNotation? = nil,
    introducer: ImplIntroducer? = nil
  ) {
    self.stem = stem
    self.labels = labels
    self.notation = notation
    self.introducer = introducer
  }

  /// Returns a textual description of `labels`.
  public static func describe(labels: [String?]) -> String {
    labels.map({ "\($0 ?? "_"):" }).joined()
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

  /// Returns `self` with `newIntroducer` incorporated into its value.
  ///
  /// - Precondition: `self` has no introducer.`
  func introduced(by newIntroducer: ImplIntroducer) -> Self {
    precondition(self.introducer == nil)
    return Name(stem: stem, labels: labels, notation: notation, introducer: newIntroducer)
  }

}

extension SourceRepresentable where Part == Name {

  /// Returns `self` with `introducer` incorporated into its value.
  ///
  /// - Precondition: `self` has no introducer.`
  func introduced(by introducer: SourceRepresentable<ImplIntroducer>) -> Self {
    return .init(
      value: self.value.introduced(by: introducer.value),
      range: self.site.extended(upTo: introducer.site.end))
  }

}
