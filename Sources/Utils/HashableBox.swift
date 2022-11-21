/// A wrapper implementing a value's conformance to `Hashable` with a custom witness.
public struct HashableBox<Base, Witness: HashableWitness<Base>>: Hashable {

  /// The value wrapped by this instance.
  public let base: Base

  /// Creates a new instance wrapping `base`.
  public init(_ base: Base) {
    self.base = base
  }

  public func hash(into hasher: inout Hasher) {
    Witness.hash(base, into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    Witness.isEqual(l.base, to: r.base)
  }

}
