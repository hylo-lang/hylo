import FrontEnd

/// Projects the address of an object.
public struct Yield: Instruction {

  /// The capability being projected.
  public let capability: AccessEffect

  /// The returned address.
  public private(set) var projection: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(capability: AccessEffect, projection: Operand, site: SourceRange) {
    self.capability = capability
    self.projection = projection
    self.site = site
  }

  public var operands: [Operand] {
    [projection]
  }

  public var successors: [Block.ID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    projection = new
  }

  func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates a `yield` anchored at `site` that projects `a` with capability `c`.
  func makeYield(_ c: AccessEffect, _ a: Operand, in f: Function.ID, at site: SourceRange) -> Yield {
    precondition(type(of: a, in: f).isAddress)
    return .init(capability: c, projection: a, site: site)
  }

}
