import Utils

/// A term variable.
public struct TermVariable: TermProtocol {

  /// The identifier of the variable.
  public let rawValue: UInt64

  /// Creates an instance with given `rawValue`.
  public init(_ rawValue: UInt64) {
    self.rawValue = rawValue
  }

  public var flags: ValueFlags { .hasVariable }

  /// The context in which this instance was created.
  var context: UInt8 { UInt8(rawValue >> 56) }

  /// The identifier of this instance.
  var identifier: UInt64 { ~(255 << 56) & rawValue }

}

extension TermVariable: CustomStringConvertible {

  public var description: String { "%\(context).\(identifier)*" }

}
