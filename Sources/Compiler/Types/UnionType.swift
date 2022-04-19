/// A union type.
public struct UnionType: TypeProtocol, Hashable {

  /// The elements of the union.
  public let elements: Set<Type>

  public let flags: TypeFlags

  /// Creates a new union type with the specified elements.
  public init<S: Sequence>(_ elements: S) where S.Element == Type {
    self.elements = Set(elements)
    self.flags = TypeFlags(merging: self.elements.map({ $0.flags }))
  }

}

extension UnionType: CustomStringConvertible {

  public var description: String {
    if elements.isEmpty {
      return "Never"
    } else {
      return elements.map({ "\($0)" }).joined(separator: " | ")
    }
  }

}
