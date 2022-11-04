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

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
