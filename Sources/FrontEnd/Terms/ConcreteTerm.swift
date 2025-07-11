import Utils

/// A box wrapping a concrete compile-time value.
public struct ConcreteTerm: TermProtocol, Sendable {

  /// The value of the term.
  public let value: AnyHashableAndSendable

  /// Creates an instance with the given value.
  public init(wrapping value: some Hashable & Sendable) {
    self.value = AnyHashableAndSendable(wrapping: value)
  }

  public var flags: ValueFlags { .init() }

}

extension ConcreteTerm: CustomStringConvertible {

  public var description: String { "\(value)" }

}
