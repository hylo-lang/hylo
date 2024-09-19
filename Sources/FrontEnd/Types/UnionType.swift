import Utils

/// A union of types.
public struct UnionType: TypeProtocol {

  // Note: The elements of the union are stored in an array rather than a set of that the internal
  // representation of a particular instance is deterministic across compiler runs, as the order in
  // which elements have been inserted typically depends on the order in which a type expression
  // has been visited. Further, most union types are small and therefore using a set wouldn't bring
  // performance benefits for most lookups.

  /// The elements of a union type.
  public typealias Elements = [AnyType]

  /// The elements of the union.
  public let elements: Elements

  public let flags: ValueFlags

  /// Creates an instance type with the specified elements.
  public init<S: Sequence>(_ elements: S) where S.Element == AnyType {
    self.elements = Array(elements)

    var fs = ValueFlags(self.elements.map(\.flags))
    if self.elements.count == 1 {
      fs.insert(.hasNonCanonical)
    }
    self.flags = fs
  }

  /// `true` if `self` is Hylo's `Never` type.
  public var isNever: Bool {
    elements.isEmpty
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    UnionType(elements.map({ $0.transform(mutating: &m, transformer) }))
  }

}

extension UnionType: Hashable {

  public func hash(into hasher: inout Hasher) {
    // Generate a seed from a snapshot of the hasher.
    var hash = read(hasher, { $0.finalize() })
    for m in elements {
      // Note: _using_ `hashValue` is not deprecated.
      hash ^= m.hashValue
    }
    hasher.combine(hash)
  }

  public static func == (l: Self, r: Self) -> Bool {
    if l.elements.count != r.elements.count { return false }
    for m in l.elements {
      if !r.elements.contains(m) { return false }
    }
    return true
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
