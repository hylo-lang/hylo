import Core

/// Returns a copy of the witness table of an existential container.
public struct CopyWitnessTableInstruction: Instruction {

  /// The existential container from which the witness table should be extracted.
  public private(set) var container: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with given properties.
  fileprivate init(container: Operand, site: SourceRange) {
    self.container = container
    self.site = site
  }

  public var types: [LoweredType] { [.object(WitnessTableType())] }

  public var operands: [Operand] { [container] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    container = new
  }

}

extension Module {

  /// Creates a `copy_witness_table` anchored at `anchor` that returns a copy `container`'s witness
  /// table.
  func makeCopyWitnessTable(
    of container: Operand, anchoredAt anchor: SourceRange
  ) -> CopyWitnessTableInstruction {
    .init(container: container, site: anchor)
  }

}
