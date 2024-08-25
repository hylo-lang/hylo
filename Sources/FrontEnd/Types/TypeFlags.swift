/// A set of type flags.
public struct TypeFlags: Hashable, OptionSet {

  public typealias RawValue = UInt8

  public let rawValue: UInt8

  public init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  /// Returns the union of `l` with `r`.
  public static func | (l: Self, r: Self) -> Self {
    l.union(r)
  }

  /// Returns the intesection of `l` with `r`.
  public static func & (l: Self, r: Self) -> Self {
    l.intersection(r)
  }

  /// The type contains one or more error types.
  public static let hasError = TypeFlags(rawValue: 1 << 0)

  /// The type contains open type variables.
  public static let hasVariable = TypeFlags(rawValue: 1 << 1)

  /// The type contains skolemized variables.
  public static let hasSkolem = TypeFlags(rawValue: 1 << 2)

  /// The type is not canonical.
  public static let hasNonCanonical = TypeFlags(rawValue: 1 << 3)

}
