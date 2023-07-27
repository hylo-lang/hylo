import Core

/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct Unrechable: Terminator {

  /// The site of the code corresponding to that instruction.
  public var site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(site: SourceRange) {
    self.site = site
  }

  public var types: [IR.`Type`] { [] }

  public var operands: [Operand] { [] }

  public var successors: [Block.ID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

  func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates an `unreachable` anchored at `site` that marks the execution path unreachable.
  func makeUnreachable(at site: SourceRange) -> Unrechable {
    .init(site: site)
  }

}
