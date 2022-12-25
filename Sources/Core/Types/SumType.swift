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
    self.flags =
      self.elements.isEmpty
      ? [.isCanonical]
      : TypeFlags(merging: self.elements.map({ $0.flags }))
  }

  public func transform(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    SumType(elements.map({ (e) -> AnyType in e.transform(transformer) }))
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
