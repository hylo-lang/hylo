/// A box wrapping a concrete compile-time value.
public struct ConcreteTerm: TermProtocol {

  /// The value of the term.
  public let value: AnyHashable

  /// Creates an instance with the given value.
  public init(wrapping value: AnyHashable) {
    self.value = value
  }

  public var flags: ValueFlags { .init() }

}

extension ConcreteTerm: CustomStringConvertible {

  public var description: String { "\(value)" }

}
