import Utils

/// A box wrapping a concrete compile-time value.
public struct ConcreteTerm: TermProtocol {
  // Todo: get rid of unsafe type erasure in SendableValue, currently it's only used with an Int value in practice.
  
  /// The value of the term.
  ///
  /// WARNING: Don't use downcasts from AnyHashableAndSendable directly, only on its wrapped `.anyHashable` object.
  /// `.sendableValue as? T` would try to downcast the user-defined AnyHashableAndSendable type, which doesn't work like downcasting `AnyHashable`.
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
