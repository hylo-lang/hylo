import Utils

/// The ID of a pattern.
public protocol PatternID: NodeIDProtocol {}

extension NodeID: PatternID where Subject: Pattern {}

/// The type-erased ID of a pattern.
public struct AnyPatternID: PatternID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from a pattern ID.
  public init<T: PatternID>(_ other: T) {
    base = AnyNodeID(other)
  }
  /// Creates an instance with the same raw value as `x` failing iff `!(x.kind is Pattern)`.
  public init?<T: NodeIDProtocol>(_ x: T) {
    if x.kind.value is Pattern.Type {
      self.base = AnyNodeID(x)
    } else {
      return nil
    }
  }

  public var rawValue: NodeRawIdentity { base.rawValue }

  public var kind: NodeKind { base.kind }

}
