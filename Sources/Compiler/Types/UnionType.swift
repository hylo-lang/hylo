/// A union type.
public struct UnionType: TypeProtocol {

  /// The elements of the union.
  public let elements: Set<AnyType>

  public let flags: TypeFlags

  /// Creates a new union type with the specified elements.
  public init<S: Sequence>(_ elements: S) where S.Element == AnyType {
    self.elements = Set(elements)
    self.flags = self.elements.isEmpty
      ? [.isCanonical]
      : TypeFlags(merging: self.elements.map({ $0.flags }))
  }

  public func transform(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    UnionType(elements.map({ (e) -> AnyType in e.transform(transformer) }))
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
