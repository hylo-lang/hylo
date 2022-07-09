/// Branches unconditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct BranchInst: Inst {

  /// The target of the branch.
  public let target: BlockID

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    let t = printer.translate(block: target)
    output.write("branch \(t)")
  }

  public var type: LoweredType { .object(.unit) }

}
