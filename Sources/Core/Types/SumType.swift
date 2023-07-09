import Utils

/// A sum type.
public struct SumType: TypeProtocol {

  /// A type representing the elements of a sum type.
  public typealias Elements = Set<AnyType>

  /// The elements of the sum.
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
    SumType(elements.map({ $0.transform(mutating: &m, transformer) }))
  }

}

extension SumType: CustomStringConvertible {

  public var description: String {
    if elements.isEmpty {
      return "Never"
    } else {
      return "Sum<\(list: elements)>"
    }
  }

}
