// Child node with introducer keyword, eg else clause in if-else
// The NodeIDProtocol conformance is used for BundledNode compability
// eg DoWhileStmt in Emitter.swift: `program[s].condition.site``
public struct Introduced<T: NodeIDProtocol>: NodeIDProtocol {
  public let introducerSite: SourceRange
  public let value: T

  public var rawValue: NodeID.RawValue { value.rawValue }
  public var kind: NodeKind { value.kind }

  public init?<Other: NodeIDProtocol>(_ x: Other) {
    if let i = x as? Introduced {
      self = i
    } else {
      return nil
    }
  }

  public init(introducerSite: SourceRange, value: T) {
    self.introducerSite = introducerSite
    self.value = value
  }
}
