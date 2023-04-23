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

  /// Inserts the specified flags.
  public mutating func insert(_ flags: TypeFlags) {
    universal = universal | flags.universal
    existential = existential | flags.existential
  }

  /// Retuns a set of flags in which `flags` have been inserted.
  public func inserting(_ flags: TypeFlags) -> TypeFlags {
    var newFlags = self
    newFlags.insert(flags)
    return newFlags
  }

  /// Removes the specified flags.
  public mutating func remove(_ flags: TypeFlags) {
    universal = universal & ~flags.universal
    existential = existential & ~flags.existential
  }

  /// Retuns a set of flags in which `flags` have been removed.
  public func removing(_ flags: TypeFlags) -> TypeFlags {
    var newFlags = self
    newFlags.remove(flags)
    return newFlags
  }

  /// Merge this set of flags with another set.
  public mutating func merge(_ flags: TypeFlags) {
    universal = universal & flags.universal
    existential = existential | flags.existential
  }

  /// Retuns a set of flags in which `flags` have been merged.
  public func merging(_ flags: TypeFlags) -> TypeFlags {
    var newFlags = self
    newFlags.merge(flags)
    return newFlags
  }

  /// The type is in canonical from.
  public static let isCanonical = TypeFlags(universal: 1 << 0, existential: 0)

  /// The type is generic.
  public static let isGeneric = TypeFlags(universal: 1 << 1, existential: 0)

  /// The type contains one or more error types.
  public static let hasError = TypeFlags(universal: 0, existential: 1 << 0)

  /// Te type contains one or more type variables.
  public static let hasVariable = TypeFlags(universal: 0, existential: 1 << 1)

  /// The type contains one or more generic type parameters.
  public static let hasGenericTypeParameter = TypeFlags(universal: 0, existential: 1 << 2)

  /// The type contains one or more generic value parameters.
  public static let hasGenericValueParameter = TypeFlags(universal: 0, existential: 1 << 3)

  /// The type contains one or more remote types.
  public static let hasRemoteType = TypeFlags(universal: 0, existential: 1 << 4)

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
