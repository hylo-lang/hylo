import Utils

/// A box wrapping a concrete compile-time value.
public struct ConcreteTerm: TermProtocol {

  // The value of the term.
  //
  // WARNING: Don't use downcasts from here, `as?` will not work properly, only on its wrapped `.anyHashable` object.
  private let sendableValue: AnyHashableAndSendable

  /// The value of the term.
  public var value: AnyHashable {
    sendableValue.anyHashable
  }

  /// Creates an instance with the given value.
  public init(wrapping value: some Hashable & Sendable) {
    self.sendableValue = AnyHashableAndSendable(wrapping: value)
  }

  public var flags: ValueFlags { .init() }

}

extension ConcreteTerm: CustomStringConvertible {

  public var description: String { "\(value)" }

}
