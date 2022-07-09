/// Branches conditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct CondBranchInst: Inst {

  /// A Boolean condition.
  public let condition: Operand

  /// The target of the branch if `condition` is true.
  public let targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public let targetIfFalse: Block.ID

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    let t = printer.translate(block: targetIfTrue)
    let f = printer.translate(block: targetIfFalse)

    output.write("cond_branch ")
    condition.dump(into: &output, with: &printer)
    output.write(", \(t), \(f)")
  }

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [] }

}
