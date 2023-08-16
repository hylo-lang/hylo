import Core

/// Ends the exposition of a captured access.
public struct CloseCapture: Instruction {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(start: Operand, site: SourceRange) {
    self.operands = [start]
    self.site = site
  }

  /// The access being closed; must be the result of an `open_capture` instruction.
  public var start: Operand {
    operands[0]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[0] = new
  }

}

extension Module {

  /// Creates a `close_capture` anchored at `site` that ends the exposition of a captured access.
  func makeCloseCapture(_ start: Operand, at site: SourceRange) -> CloseCapture {
    precondition(start.instruction.map({ self[$0] is OpenCapture }) ?? false)
    return .init(start: start, site: site)
  }

}
