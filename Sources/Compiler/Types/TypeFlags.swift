/// A set of type flags.
public struct TypeFlags: Hashable {

  /// Universal flags.
  private var universal: UInt8

  /// Existential flags.
  private var existential: UInt8

  private init(universal: UInt8, existential: UInt8) {
    self.universal = universal
    self.existential = existential
  }

  /// Create a new set of type flags that merges all sets in `elements`.
  public init<C: Collection>(merging elements: C) where C.Element == TypeFlags {
    if let first = elements.first {
      self = elements.dropFirst().reduce(into: first, { (a, b) in a.merge(b) })
    } else {
      self.init(universal: 0, existential: 0)
    }
  }

  /// Returns whether the set contains all the specified flags.
  public func contains(_ flags: TypeFlags) -> Bool {
    (universal & flags.universal == flags.universal)
      && (existential & flags.existential == flags.existential)
  }

  /// Merge this set of flags with another set.
  public mutating func merge(_ other: TypeFlags) {
    universal = universal & other.universal
    existential = existential | other.existential
  }

  /// The type is canonical from.
  public static let isCanonical         = TypeFlags(universal: 1 << 0, existential: 0)

  /// The type contains one or more error types.
  public static let hasError            = TypeFlags(universal: 0, existential: 1 << 0)

  /// Te type contains one or more type variables.
  public static let hasVariable         = TypeFlags(universal: 0, existential: 1 << 1)

  /// The type contains one or more generic size parameters.
  public static let hasGenericSizeParam = TypeFlags(universal: 0, existential: 1 << 2)

  /// The type contains one or more generic type parameters.
  public static let hasGenericTypeParam = TypeFlags(universal: 0, existential: 1 << 3)

}

extension TypeFlags: ExpressibleByArrayLiteral {

  public init(arrayLiteral elements: TypeFlags...) {
    universal = 0
    existential = 0
    for e in elements {
      universal |= e.universal
      existential |= e.existential
    }
  }

}
