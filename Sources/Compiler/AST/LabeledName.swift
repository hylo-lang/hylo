/// A labeled name (e.g., `foo(bar:_:)` or `infix+`).
public struct LabeledName {

  /// The base of the name.
  var base: String

  // The argument labels of the name.
  var labels: [String]

  /// The operator notation of the name.
  var notation: OperatorNotation?

  public init(base: String, labels: [String] = [], notation: OperatorNotation? = nil) {
    self.base = base
    self.labels = labels
    self.notation = notation
  }

  static let empty = LabeledName(base: "")

  public static func == (lhs: LabeledName, rhs: String) -> Bool {
    return lhs.labels.isEmpty && (lhs.notation == nil) && (lhs.base == rhs)
  }

}

extension LabeledName: Hashable {}

extension LabeledName: CustomStringConvertible {

  public var description: String {
    var result = ""
    if let n = notation {
      result.append("\(n)")
    }

    result.append(base)

    if !labels.isEmpty {
      result.append("(")
      result.append(labels.map(String.init(describing:)).joined(separator: ":"))
      result.append(")")
    }

    return result
  }

}
