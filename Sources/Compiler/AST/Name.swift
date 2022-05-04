/// An unqualified name denoting an entity.
public struct Name: Hashable {

  /// The stem identifier of the referred entity.
  public var stem: Identifier

  /// The argument labels of the referred entity, .
  public var labels: [String?]

  /// The operator notation of the referred entity.
  public var notation: OperatorNotation?

  /// Creates a new name.
  public init(stem: Identifier, labels: [String?] = []) {
    self.stem = stem
    self.labels = labels
    self.notation = nil
  }

  /// Creates a new operator name.
  public init(stem: Identifier, notation: OperatorNotation) {
    self.stem = stem
    self.labels = []
    self.notation = notation
  }

}

extension Name: CustomStringConvertible {

  public var description: String {
    if let notation = notation {
      return "\(notation)\(stem)"
    } else if labels.isEmpty {
      return stem
    } else {
      let labels = labels.reduce(into: "", { (s, l) in s += (l ?? "_") + ":" })
      return "\(stem)(\(labels))"
    }
  }

}
