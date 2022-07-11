// Borrows an access on a record member.
public struct BorrowMemberInst: Inst {

  /// The value of the record whose member address is computed.
  public let value: Operand

  /// The indices of the index member.
  ///
  /// - Requires: This array cannot be empty.
  public let path: [Int]

  public let type: LoweredType

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("borrow_member ")
    value.dump(into: &output, with: &printer)
    output.write(", \(path.descriptions())")
  }

  public var operands: [Operand] { [value] }

}
