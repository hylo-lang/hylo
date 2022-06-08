/// An unqualified name denoting an entity.
public struct Name: Hashable {

  /// The stem identifier of the referred entity.
  public var stem: Identifier

  /// The argument labels of the referred entity, given that it is a function.
  public var labels: [String?]

  /// The operator notation of the referred entity, given that it is an operator.
  public var notation: OperatorNotation?

  /// The method introducer of the referred entity, given that it is a method implementation.
  public var introducer: MethodImplDecl.Introducer?

  /// Creates a new name.
  public init(
    stem: Identifier,
    labels: [String?] = [],
    introducer: MethodImplDecl.Introducer? = nil
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
    introducer: MethodImplDecl.Introducer? = nil
  ) {
    self.stem = stem
    self.labels = []
    self.notation = notation
    self.introducer = introducer
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
