import FrontEnd

/// A return instruction.
public struct Return: Terminator {

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(site: SourceRange) {
    self.site = site
  }

  public var successors: [Block.AbsoluteID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

  func replaceSuccessor(_ old: Block.AbsoluteID, with new: Block.AbsoluteID) -> Bool {
    false
  }

}

extension Module {

  /// Creates a `return` anchored at `site`.
  func makeReturn(at site: SourceRange) -> Return {
    .init(site: site)
  }

}
