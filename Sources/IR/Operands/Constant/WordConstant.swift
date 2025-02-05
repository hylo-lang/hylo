import FrontEnd

// An integer constant that has the size of a machine word.
public struct WordConstant: Constant, Hashable, Sendable {

  /// The value of the constant.
  ///
  /// The value is stored as a 64-bit integer with the assumption that no supported target use a
  /// longer machine word.
  public let value: Int64

  /// Creates a new constant with given `value`.
  public init(_ value: Int) {
    self.value = .init(truncatingIfNeeded: value)
  }

  public var type: IR.`Type` { .object(BuiltinType.word) }

}

extension WordConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(\(value))"
  }

}
