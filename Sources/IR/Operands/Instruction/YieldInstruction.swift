import Core

/// Projects the address of an object.
public struct YieldInstruction: Instruction {

  /// The capability being projected.
  public let capability: AccessEffect

  /// The returned address.
  public let projection: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(capability: AccessEffect, projection: Operand, site: SourceRange) {
    self.capability = capability
    self.projection = projection
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [projection] }

  public var successors: [Block.ID] { [] }

  func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates a `yield` anchored at `anchor` that projects `a` with capability `c`.
  func makeYield(
    _ c: AccessEffect, _ a: Operand, anchoredAt anchor: SourceRange
  ) -> YieldInstruction {
    precondition(type(of: a).isAddress)
    return .init(capability: c, projection: a, site: anchor)
  }

}
