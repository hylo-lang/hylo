/// A wrapper implementing a value's conformance to `Hashable` with a custom witness.
public struct HashableBox<Witness: HashableWitness>: Hashable, Sendable where Witness.Element: Sendable {

  /// The value wrapped by this instance.
  public let base: Witness.Element

  /// Creates a new instance wrapping `base`.
  public init(_ base: Witness.Element) {
    self.base = base
  }

  public func hash(into hasher: inout Hasher) {
    Witness.hash(base, into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    Witness.isEqual(l.base, to: r.base)
  }

}
