/// Allocates memory on the stack.
public struct AllocStackInst: Inst {

  /// The type of the object for which storage is allocated.
  public let objectType: Type

  public var type: LoweredType { .address(objectType) }

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("alloc_stack \(objectType)")
  }

  public var operands: [Operand] { [] }

}
