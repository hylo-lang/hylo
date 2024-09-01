/// Properties about the representation of a type or term.
public struct ValueFlags: Hashable, OptionSet {

  public typealias RawValue = UInt8

  public let rawValue: UInt8

  public init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  /// Returns the union of `l` with `r`.
  public static func | (l: Self, r: Self) -> Self {
    l.union(r)
  }

  /// The type contains one or more error types.
  public static let hasError = ValueFlags(rawValue: 1 << 0)

  /// The type contains open type variables.
  public static let hasVariable = ValueFlags(rawValue: 1 << 1)

  /// The type contains skolemized variables.
  public static let hasSkolem = ValueFlags(rawValue: 1 << 2)

  /// The type is not canonical.
  public static let hasNonCanonical = ValueFlags(rawValue: 1 << 3)

}
