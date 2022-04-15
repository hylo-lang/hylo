/// A tuple type.
public struct TupleType: TypeProtocol, Hashable {

  /// An element in a tuple type.
  public struct Element: Hashable {

    /// The label of the element.
    public let label: String?

    /// The type of the element.
    public let type: Type

  }

  /// The elements of the tuple.
  public let elements: [Element]

  public let flags: TypeFlags

  /// Creates a tuple type with a sequence of elements.
  public init<S: Sequence>(_ elements: S) where S.Element == Element {
    self.elements = Array(elements)

    var fs = TypeFlags(merging: self.elements.map({ $0.type.flags }))
    switch self.elements.count {
    case 0:
      fs.insert(.isCanonical)
    case 1:
      fs.remove(.isCanonical)
    default:
      break
    }
    flags = fs
  }

  /// Creates a tuple of unlabeled elements with the types in the given sequence.
  public init<S: Sequence>(types: S) where S.Element == Type {
    self.init(types.map({ Element(label: nil, type: $0) }))
  }

}

extension TupleType: CustomStringConvertible {

  public var description: String {
    let elements = elements.map({ "\($0)" }).joined(separator: ", ")
    return "(\(elements))"
  }

}

extension TupleType.Element: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
