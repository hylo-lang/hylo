import FrontEnd

/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct Unreachable: Terminator {

  /// The site of the code corresponding to that instruction.
  public var site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(site: SourceRange) {
    self.site = site
  }

  /// Creates an instance anchored at `site` that marks the execution path unreachable.
  init(at site: SourceRange) {
    self.site = site
  }

  public var successors: [Block.ID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

  func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
    false
  }

}

extension Function {

  /// Creates an `unreachable` anchored at `site` that marks the execution path unreachable.
  func makeUnreachable(at site: SourceRange) -> Unreachable {
    .init(site: site)
  }

  /// Creates an `unreachable` anchored at `site` that marks the execution path unreachable, inserting it at `p`.
  @discardableResult
  mutating func makeUnreachable(at site: SourceRange, insertingAt p: InsertionPoint) -> InstructionID {
    insert(makeUnreachable(at: site), at: p)
  }

}
