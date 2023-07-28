import Core

/// Close a previously created access to the payload of a union.
public struct CloseUnion: Instruction {

  /// The access being closed; must be the result of an `open_union` instruction.
  public private(set) var start: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(start: Operand, site: SourceRange) {
    self.start = start
    self.site = site
  }

  public var operands: [Operand] {
    [start]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    start = new
  }

}

extension Module {

  /// Creates an `close_union` anchored at `site` that ends an access to the payload of a union
  /// opened previously by `start`.
  func makeCloseUnion(_ start: Operand, at site: SourceRange) -> CloseUnion {
    precondition(start.instruction.map({ self[$0] is OpenUnion }) ?? false)
    return .init(start: start, site: site)
  }

}
