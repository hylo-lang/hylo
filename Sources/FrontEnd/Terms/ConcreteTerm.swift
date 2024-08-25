/// A box wrapping a concrete compile-time value.
public struct ConcreteTerm: TermProtocol {

  /// The value of the term.
  public let value: AnyHashable

  /// Creates an instance with the given value.
  public init(wrapping value: AnyHashable) {
    self.value = value
  }

}

extension ConcreteTerm: CustomStringConvertible {

  public var description: String { "\(value)" }

}
