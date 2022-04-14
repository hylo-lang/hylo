/// A union a type.
public struct UnionType: TypeProtocol, Hashable {

  /// The elements of the union.
  public let elements: Set<Type>

  public let flags: TypeFlags

  /// Creates a new union type with the specified elements.
  public init<S: Sequence>(elements: S) where S.Element == Type {
    self.elements = Set(elements)
    self.flags = TypeFlags(merging: self.elements.map({ $0.flags }))
  }

}
