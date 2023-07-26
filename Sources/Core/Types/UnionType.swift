import Utils

/// A union of types.
public struct UnionType: TypeProtocol {

  /// The elements of a union type.
  public typealias Elements = Set<AnyType>

  /// The elements of the union.
  public let elements: Elements

  public let flags: TypeFlags

  /// Creates an instance type with the specified elements.
  public init<S: Sequence>(_ elements: S) where S.Element == AnyType {
    self.elements = Set(elements)

    let f = TypeFlags(merging: self.elements.map({ $0.flags }))
    switch self.elements.count {
    case 0:
      self.flags = .isCanonical
    case 1:
      self.flags = f.removing(.isCanonical)
    default:
      self.flags = f
    }
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    UnionType(elements.map({ $0.transform(mutating: &m, transformer) }))
  }

}

extension UnionType: CustomStringConvertible {

  public var description: String {
    if elements.isEmpty {
      return "Never"
    } else {
      return "Union<\(list: elements)>"
    }
  }

}
