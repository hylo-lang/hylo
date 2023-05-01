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
  public let introducer: AccessEffect?

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

  /// Creates an instance with the given properties.
  public init(
    stem: Identifier,
    labels: [String?],
    notation: OperatorNotation? = nil,
    introducer: AccessEffect? = nil
  ) {
    self.stem = stem
    self.labels = labels
    self.notation = notation
    self.introducer = introducer
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init?(of d: FunctionDecl.ID, in ast: AST) {
    guard let stem = ast[d].identifier?.value else { return nil }
    if let notation = ast[d].notation?.value {
      self.init(stem: stem, notation: notation)
    } else {
      self.init(stem: stem, labels: ast[ast[d].parameters].map(\.label?.value))
    }
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init(of d: InitializerDecl.ID, in ast: AST) {
    self.init(stem: "init", labels: ast[ast[d].parameters].map(\.label?.value))
  }

  /// Creates the name introduced by `decl` in `ast`.
  public init(of d: MethodDecl.ID, in ast: AST) {
    let stem = ast[d].identifier.value
    if let notation = ast[d].notation?.value {
      self.init(stem: stem, notation: notation)
    } else {
      self.init(stem: stem, labels: ast[ast[d].parameters].map(\.label?.value))
    }
  }

  /// Returns `self` appending `x` or `nil` if `self` already has an introducer.
  public func appending(_ x: AccessEffect) -> Name? {
    introducer == nil
      ? Name(stem: stem, labels: labels, notation: notation, introducer: x)
      : nil
  }

  /// Returns a textual description of `labels`.
  public static func describe<S: Sequence<String?>>(labels: S) -> String {
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
  func introduced(by newIntroducer: AccessEffect) -> Self {
    precondition(self.introducer == nil)
    return Name(stem: stem, labels: labels, notation: notation, introducer: newIntroducer)
  }

}

extension SourceRepresentable where Part == Name {

  /// Returns `self` with `introducer` incorporated into its value.
  ///
  /// - Precondition: `self` has no introducer.`
  func introduced(by introducer: SourceRepresentable<AccessEffect>) -> Self {
    return .init(
      value: self.value.introduced(by: introducer.value),
      range: self.site.extended(upTo: introducer.site.end))
  }

}
