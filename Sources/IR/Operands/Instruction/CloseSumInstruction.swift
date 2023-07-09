import Core

/// Close a previously created access to the payload of a sum.
public struct CloseSumInstruction: Instruction {

  /// The access being closed; must be the result of an `open_sum` instruction.
  public private(set) var start: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(start: Operand, site: SourceRange) {
    self.start = start
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [start] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    start = new
  }

}

extension Module {

  /// Creates an `close_sum` anchored at `site` that ends an access to the payload of a sum opened
  /// previously by `start`.
  func makeCloseSum(_ start: Operand, at site: SourceRange) -> CloseSumInstruction {
    precondition(start.instruction.map({ self[$0] is OpenSumInstruction }) ?? false)
    return .init(start: start, site: site)
  }

}
