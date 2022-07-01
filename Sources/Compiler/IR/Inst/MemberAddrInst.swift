/// Computes the address of a stored member in a record.
public struct MemberAddrInst: Inst {

  /// The value of the record whose member address is computed.
  public let value: Operand

  /// The offset of the member.
  public let offset: Int

  public let type: IRType

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("member_addr ")
    value.dump(into: &output, with: &printer)
    output.write(", \(offset)")
  }

}
